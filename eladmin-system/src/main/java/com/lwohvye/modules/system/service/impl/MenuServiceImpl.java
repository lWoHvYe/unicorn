/*
 *  Copyright 2019-2020 Zheng Jie
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.lwohvye.modules.system.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.ObjectUtil;
import com.lwohvye.context.CycleAvoidingMappingContext;
import com.lwohvye.exception.BadRequestException;
import com.lwohvye.exception.EntityExistException;
import com.lwohvye.modules.system.domain.Menu;
import com.lwohvye.modules.system.domain.Role;
import com.lwohvye.modules.system.domain.User;
import com.lwohvye.modules.system.domain.vo.MenuMetaVo;
import com.lwohvye.modules.system.domain.vo.MenuVo;
import com.lwohvye.modules.system.repository.MenuRepository;
import com.lwohvye.modules.system.repository.UserRepository;
import com.lwohvye.modules.system.service.IMenuService;
import com.lwohvye.modules.system.service.IRoleService;
import com.lwohvye.modules.system.service.dto.MenuDto;
import com.lwohvye.modules.system.service.dto.MenuQueryCriteria;
import com.lwohvye.modules.system.service.dto.RoleSmallDto;
import com.lwohvye.modules.system.service.mapstruct.MenuMapper;
import com.lwohvye.utils.*;
import com.lwohvye.utils.redis.RedisUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.*;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;

/**
 * @author Zheng Jie
 */
@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "menu")
public class MenuServiceImpl implements IMenuService {

    private final MenuRepository menuRepository;
    private final UserRepository userRepository;
    private final MenuMapper menuMapper;
    private final IRoleService roleService;
    private final RedisUtils redisUtils;

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class)
    public List<MenuDto> queryAll(MenuQueryCriteria criteria, Boolean isQuery) throws Exception {
        Sort sort = Sort.by(Sort.Direction.ASC, "menuSort");
        if (Boolean.TRUE.equals(isQuery)) {
            criteria.setPidIsNull(true);
            List<Field> fields = QueryHelp.getAllFields(criteria.getClass(), new ArrayList<>());
            for (Field field : fields) {
                //设置对象的访问权限，保证对private的属性的访问
                field.setAccessible(true);
                Object val = field.get(criteria);
                if ("pidIsNull".equals(field.getName())) {
                    continue;
                }
                if (ObjectUtil.isNotNull(val)) {
                    criteria.setPidIsNull(null);
                    break;
                }
            }
        }
        return menuMapper.toDto(menuRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), sort), new CycleAvoidingMappingContext());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @Cacheable(key = " #root.target.getSysName() + 'id:' + #p0")
    public MenuDto findById(long id) {
        Menu menu = menuRepository.findById(id).orElseGet(Menu::new);
        ValidationUtil.isNull(menu.getId(), "Menu", "id", id);
        return menuMapper.toDto(menu, new CycleAvoidingMappingContext());
    }

    /**
     * 用户角色改变时需清理缓存
     *
     * @param currentUserId /
     * @return /
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    @Cacheable(key = " #root.target.getSysName() + 'user:' + #p0")
    public List<MenuDto> findByUser(Long currentUserId) {
        List<RoleSmallDto> roles = roleService.findByUsersId(currentUserId);
        Set<Long> roleIds = roles.stream().map(RoleSmallDto::getId).collect(Collectors.toSet());
        LinkedHashSet<Menu> menus = menuRepository.findByRoleIdsAndTypeNot(roleIds, 2);
        return menus.stream().map(menu -> menuMapper.toDto(menu, new CycleAvoidingMappingContext())).toList();
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void create(Menu resources) {
        if (menuRepository.findByTitle(resources.getTitle()) != null) {
            throw new EntityExistException(Menu.class, "title", resources.getTitle());
        }
        if (StringUtils.isNotBlank(resources.getComponentName())) {
            if (menuRepository.findByComponentName(resources.getComponentName()) != null) {
                throw new EntityExistException(Menu.class, "componentName", resources.getComponentName());
            }
        }
        if (resources.getPid().equals(0L)) {
            resources.setPid(null);
        }
        if (resources.getIFrame()) {
            String http = "http://", https = "https://";
            if (!(resources.getPath().toLowerCase().startsWith(http) || resources.getPath().toLowerCase().startsWith(https))) {
                throw new BadRequestException("外链必须以http://或者https://开头");
            }
        }
        menuRepository.save(resources);
        // 计算子节点数目
        resources.setSubCount(0);
        // 更新父节点菜单数目
        updateSubCnt(resources.getPid());
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void update(Menu resources) {
        if (resources.getId().equals(resources.getPid())) {
            throw new BadRequestException("上级不能为自己");
        }
        Menu menu = menuRepository.findById(resources.getId()).orElseGet(Menu::new);
        ValidationUtil.isNull(menu.getId(), "Permission", "id", resources.getId());

        if (resources.getIFrame()) {
            String http = "http://", https = "https://";
            if (!(resources.getPath().toLowerCase().startsWith(http) || resources.getPath().toLowerCase().startsWith(https))) {
                throw new BadRequestException("外链必须以http://或者https://开头");
            }
        }
        Menu menu1 = menuRepository.findByTitle(resources.getTitle());

        if (menu1 != null && !menu1.getId().equals(menu.getId())) {
            throw new EntityExistException(Menu.class, "title", resources.getTitle());
        }

        if (resources.getPid().equals(0L)) {
            resources.setPid(null);
        }

        // 记录的父节点ID
        Long oldPid = menu.getPid();
        Long newPid = resources.getPid();

        if (StringUtils.isNotBlank(resources.getComponentName())) {
            menu1 = menuRepository.findByComponentName(resources.getComponentName());
            if (menu1 != null && !menu1.getId().equals(menu.getId())) {
                throw new EntityExistException(Menu.class, "componentName", resources.getComponentName());
            }
        }
        menu.setTitle(resources.getTitle());
        menu.setComponent(resources.getComponent());
        menu.setPath(resources.getPath());
        menu.setIcon(resources.getIcon());
        menu.setIFrame(resources.getIFrame());
        menu.setPid(resources.getPid());
        menu.setMenuSort(resources.getMenuSort());
        menu.setCache(resources.getCache());
        menu.setHidden(resources.getHidden());
        menu.setComponentName(resources.getComponentName());
        menu.setPermission(resources.getPermission());
        menu.setType(resources.getType());
        menuRepository.save(menu);
        // 计算父级菜单节点数目
        updateSubCnt(oldPid);
        updateSubCnt(newPid);
        // 清理缓存
        delCaches(resources.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Set<Menu> getChildMenus(List<Menu> menuList, Set<Menu> menuSet) {
        for (Menu menu : menuList) {
            menuSet.add(menu);
            List<Menu> menus = menuRepository.findByPid(menu.getId());
            if (menus != null && !menus.isEmpty()) {
                getChildMenus(menus, menuSet);
            }
        }
        return menuSet;
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public void delete(Set<Menu> menuSet) {
        for (Menu menu : menuSet) {
            // 清理缓存
            delCaches(menu.getId());
            roleService.untiedMenu(menu.getId());
            menuRepository.deleteById(menu.getId());
            updateSubCnt(menu.getPid());
        }
    }

    @Override
    @CacheEvict(allEntries = true)
    @Transactional(rollbackFor = Exception.class)
    public List<MenuDto> getMenus(Long pid) {
        List<Menu> menus;
        if (pid != null && !pid.equals(0L)) {
            menus = menuRepository.findByPid(pid);
        } else {
            menus = menuRepository.findByPidIsNull();
        }
        return menuMapper.toDto(menus, new CycleAvoidingMappingContext());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<MenuDto> getSuperior(MenuDto menuDto, List<Menu> menus) {
        if (menuDto.getPid() == null) {
            menus.addAll(menuRepository.findByPidIsNull());
            return menuMapper.toDto(menus, new CycleAvoidingMappingContext());
        }
        menus.addAll(menuRepository.findByPid(menuDto.getPid()));
        return getSuperior(findById(menuDto.getPid()), menus);
    }

    // 一次查询，聚合 + 单次循环。效率较优
    @Override
    public List<MenuDto> buildTree(List<MenuDto> menuDtos) {
        // 这里主要是利用了实体是引用传递的理念。在将实体add进集合后，对原实体对修改，对集合中对实体同样生效（因为指向同一内存地址）
        // 较传统的一级一级递归查询，效率更高  1次查询 + n^2次循环 与 1 + 1+n 次查询 的差异
        // 但双层循环，执行了 n^2 次，待优化
        // 一次优化，将双层循环 调整成 一次聚合 + 单层循环（聚合的时间开销并不大，随着数据量的增加，效率提升明显，空间换时间）
        var trees = new CopyOnWriteArrayList<MenuDto>();
//        Set<Long> ids = new HashSet<>();
        // 根据上级id做聚合
        var listMap = menuDtos.parallelStream()
                .filter(menuDto -> {
                    var pid = menuDto.getPid();
                    if (ObjectUtil.isNotNull(pid))
                        return true;
                    trees.add(menuDto);
                    return false;
                }).collect(Collectors.groupingBy(MenuDto::getPid));
        // 遍历并设置 子级
        // peek、map不会修改遍历的对象，需要将最终的结果进行赋值
        // forEach是操作的遍历的对象，对于引用类型的实体，需要注意这一点
        // 下面的逻辑里，要用forEach
        menuDtos.parallelStream().forEach(menuDto -> {
            var id = menuDto.getId();
            var childList = listMap.get(id);
            if (CollUtil.isNotEmpty(childList))
                menuDto.setChildren(childList);
        });

        // 老版本可能第一级的pid = 0，下面是兼容原数据，新版第一级是null，统一后，下面的逻辑可移除
//        if (trees.isEmpty()) {
//            return menuDtos.stream().filter(s -> !ids.contains(s.getId())).sorted(Comparator.comparing(MenuDto::getMenuSort)).toList();
//        }
        return trees.stream().sorted(Comparator.comparing(MenuDto::getMenuSort)).toList();
    }

    // 一次查询，嵌套循环处理。大幅减少数据库IO
    @Override
    public List<MenuDto> buildTree2(List<MenuDto> menuDtos) {
        // 这里主要是利用了实体是引用传递的理念。在将实体add进集合后，对原实体对修改，对集合中对实体同样生效（因为指向同一内存地址）
        // 较传统的一级一级递归查询，效率更高  1次查询 + n^2次循环 与 1 + 1+n 次查询 的差异
        // 但双层循环，执行了 n^2 次，待优化
        var trees = new CopyOnWriteArrayList<MenuDto>();
        Set<Long> ids = new HashSet<>();
        menuDtos.parallelStream().forEach(menuDto -> {
            if (menuDto.getPid() == null) {
                trees.add(menuDto);
            }
            for (MenuDto it : menuDtos) {
                if (menuDto.getId().equals(it.getPid())) {
                    if (menuDto.getChildren() == null) {
                        menuDto.setChildren(new ArrayList<>());
                    }
                    menuDto.getChildren().add(it);
                    ids.add(it.getId());
                }
            }
        });
        if (trees.isEmpty()) {
            return menuDtos.stream().filter(s -> !ids.contains(s.getId())).sorted(Comparator.comparing(MenuDto::getMenuSort)).toList();
        }
        return trees.stream().sorted(Comparator.comparing(MenuDto::getMenuSort)).toList();
    }

    // 递归，多次调用数据库。不建议
    @Override
    public List<MenuDto> buildTree3(Long currentUserId) {
        List<RoleSmallDto> roles = roleService.findByUsersId(currentUserId);
        Set<Long> roleIds = roles.stream().map(RoleSmallDto::getId).collect(Collectors.toSet());
        return menuRepository.findByRoleIdsAndPidIsNullAndTypeNot(roleIds, 2).orElseGet(LinkedHashSet::new)
                .parallelStream().map(menu -> {
                    var dto = menuMapper.toDto(menu, new CycleAvoidingMappingContext());
                    var id = dto.getId();
                    dto.setChildren(getChild4CurUser(id, roleIds));
                    return dto;
                }).toList();

    }

    private List<MenuDto> getChild4CurUser(Long pid, Set<Long> roleIds) {
        return menuRepository.findByRoleIdsAndPidAndTypeNot(roleIds, pid, 2).orElseGet(LinkedHashSet::new)
                .parallelStream().map(menu -> {
                    var dto = menuMapper.toDto(menu, new CycleAvoidingMappingContext());
                    var id = dto.getId();
                    dto.setChildren(getChild4CurUser(id, roleIds));
                    return dto;
                }).toList();
    }

    @Override
    public List<MenuVo> buildMenus(List<MenuDto> menuDtos) {
        List<MenuVo> list = new CopyOnWriteArrayList<>();
        menuDtos.parallelStream().forEach(menuDTO -> {
                    if (menuDTO != null) {
                        List<MenuDto> menuDtoList = menuDTO.getChildren();
                        MenuVo menuVo = new MenuVo();
                        menuVo.setName(ObjectUtil.isNotEmpty(menuDTO.getComponentName()) ? menuDTO.getComponentName() : menuDTO.getTitle());
                        // 一级目录需要加斜杠，不然会报警告
                        menuVo.setPath(menuDTO.getPid() == null ? "/" + menuDTO.getPath() : menuDTO.getPath());
                        menuVo.setHidden(menuDTO.getHidden());
                        // 如果不是外链
                        if (Boolean.FALSE.equals(menuDTO.getIFrame())) {
                            if (menuDTO.getPid() == null) {
                                menuVo.setComponent(StringUtils.isEmpty(menuDTO.getComponent()) ? "Layout" : menuDTO.getComponent());
                                // 如果不是一级菜单，并且菜单类型为目录，则代表是多级菜单
                            } else if (menuDTO.getType() == 0) {
                                menuVo.setComponent(StringUtils.isEmpty(menuDTO.getComponent()) ? "ParentView" : menuDTO.getComponent());
                            } else if (StringUtils.isNoneBlank(menuDTO.getComponent())) {
                                menuVo.setComponent(menuDTO.getComponent());
                            }
                        }
                        menuVo.setMeta(new MenuMetaVo(menuDTO.getTitle(), menuDTO.getIcon(), !menuDTO.getCache()));
                        if (CollectionUtil.isNotEmpty(menuDtoList)) {
                            menuVo.setAlwaysShow(true);
                            menuVo.setRedirect("noredirect");
                            menuVo.setChildren(buildMenus(menuDtoList));
                            // 处理是一级菜单并且没有子菜单的情况
                        } else if (menuDTO.getPid() == null) {
                            MenuVo menuVo1 = new MenuVo();
                            menuVo1.setMeta(menuVo.getMeta());
                            // 非外链
                            if (Boolean.FALSE.equals(menuDTO.getIFrame())) {
                                menuVo1.setPath("index");
                                menuVo1.setName(menuVo.getName());
                                menuVo1.setComponent(menuVo.getComponent());
                            } else {
                                menuVo1.setPath(menuDTO.getPath());
                            }
                            menuVo.setName(null);
                            menuVo.setMeta(null);
                            menuVo.setComponent("Layout");
                            List<MenuVo> list1 = new ArrayList<>();
                            list1.add(menuVo1);
                            menuVo.setChildren(list1);
                        }
                        list.add(menuVo);
                    }
                }
        );
        return list;
    }

    @Override
    @Cacheable
    @Transactional(rollbackFor = Exception.class)
    public Menu findOne(Long id) {
        Menu menu = menuRepository.findById(id).orElseGet(Menu::new);
        ValidationUtil.isNull(menu.getId(), "Menu", "id", id);
        return menu;
    }

    @Override
    public void download(List<MenuDto> menuDtos, HttpServletResponse response) throws IOException {
        List<Map<String, Object>> list = new ArrayList<>();
        for (MenuDto menuDTO : menuDtos) {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("菜单标题", menuDTO.getTitle());
            map.put("菜单类型", menuDTO.getType() == null ? "目录" : menuDTO.getType() == 1 ? "菜单" : "按钮");
            map.put("权限标识", menuDTO.getPermission());
            map.put("外链菜单", menuDTO.getIFrame() ? "是" : "否");
            map.put("菜单可见", menuDTO.getHidden() ? "否" : "是");
            map.put("是否缓存", menuDTO.getCache() ? "是" : "否");
            map.put("创建日期", menuDTO.getCreateTime());
            list.add(map);
        }
        FileUtil.downloadExcel(list, response);
    }

    private void updateSubCnt(Long menuId) {
        if (menuId != null) {
            int count = menuRepository.countByPid(menuId);
            menuRepository.updateSubCntById(count, menuId);
        }
    }

    /**
     * 清理缓存
     *
     * @param id 菜单ID
     */
    public void delCaches(Long id) {
        List<User> users = userRepository.findByMenuId(id);
        redisUtils.delete(CacheKey.MENU_ID + id);
        redisUtils.delByKeys4Business(CacheKey.MENU_USER, users.stream().map(User::getId).collect(Collectors.toSet()));
        // 清除 Role 缓存
        List<Role> roles = roleService.findInMenuId(new ArrayList<>() {
            {
                add(id);
            }
        });
        redisUtils.delByKeys4Business(CacheKey.ROLE_ID, roles.stream().map(Role::getId).collect(Collectors.toSet()));
    }
}
