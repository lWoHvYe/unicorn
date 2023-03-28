/*
 *    Copyright (c) 2023.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package sample.repo;

import org.springframework.stereotype.Repository;
import sample.pojo.CustomizeUser;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Repository
public class CustomizeUserInfoRepository {

    private final Map<String, CustomizeUser> memUserInfo = new HashMap<>();

    public CustomizeUserInfoRepository() {
        this.memUserInfo.put("admin", createUser("admin"));
        this.memUserInfo.put("bonus", createUser("bonus"));
    }

    public CustomizeUser findByUsername(String username) {
        return this.memUserInfo.get(username);
    }

    private static CustomizeUser createUser(String username) {
        var user = new CustomizeUser();
        user.setUsername(username);
        user.setNickname("idol");
        user.setGender("female");
        user.setPassword("password");
        user.setProfile("https://github.com/" + username);
        user.setEmail(username + "@gmail.com");
        user.setPhoneNumber("+1 (604) 555-1234;ext=5678");
        user.setRoles(List.of("admin".equals(username) ? "ADMIN" : "USER", "RES", "DIVIDEND"));
        return user;
    }
}
