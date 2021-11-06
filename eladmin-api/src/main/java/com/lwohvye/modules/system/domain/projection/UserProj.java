package com.lwohvye.modules.system.domain.projection;

import org.springframework.beans.factory.annotation.Value;

import java.util.Set;

public interface UserProj {

    Long getId();

    Set<RoleProj> getRoles();

    Set<BaseProj> getJobs();

    BaseProj getDept();

    String getUsername();

    String getNickName();

    String getEmail();

    String getPhone();

    String getGender();

    Boolean getEnabled();

    String getDescription();

    @Value("#{target.username + ' ' + target.phone}")
    String getUserInfo();

    default String getUserDetail() {
        return getUsername().concat(" ").concat(getPhone());
    }
}
