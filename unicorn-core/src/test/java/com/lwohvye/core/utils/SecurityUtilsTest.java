/*
 *    Copyright (c) 2026.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.core.utils;

import com.lwohvye.core.exception.BadRequestException;
import org.junit.jupiter.api.Test;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.Collection;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SecurityUtilsTest {

    static class TestUser {
        private Long id;

        TestUser(Long id) {
            this.id = id;
        }

        public Long getId() {
            return id;
        }
    }

    static class TestUserDetails implements UserDetails {
        private final TestUser user;

        TestUserDetails(Long id) {
            this.user = new TestUser(id);
        }

        public TestUser getUser() {
            return user;
        }

        @Override
        public Collection<? extends GrantedAuthority> getAuthorities() {
            return List.of();
        }

        @Override
        public String getPassword() {
            return "";
        }

        @Override
        public String getUsername() {
            return "tester";
        }

        @Override
        public boolean isAccountNonExpired() {
            return true;
        }

        @Override
        public boolean isAccountNonLocked() {
            return true;
        }

        @Override
        public boolean isCredentialsNonExpired() {
            return true;
        }

        @Override
        public boolean isEnabled() {
            return true;
        }
    }

    static class NoUserDetails implements UserDetails {
        @Override
        public Collection<? extends GrantedAuthority> getAuthorities() {
            return List.of();
        }

        @Override
        public String getPassword() {
            return "";
        }

        @Override
        public String getUsername() {
            return "tester";
        }

        @Override
        public boolean isAccountNonExpired() {
            return true;
        }

        @Override
        public boolean isAccountNonLocked() {
            return true;
        }

        @Override
        public boolean isCredentialsNonExpired() {
            return true;
        }

        @Override
        public boolean isEnabled() {
            return true;
        }
    }

    @Test
    void extractsNestedUserIdWithoutJsonSerialization() {
        assertEquals(42L, SecurityUtils.extractUserId(new TestUserDetails(42L)));
    }

    @Test
    void missingNestedUserThrowsBadRequest() {
        assertThrows(BadRequestException.class, () -> SecurityUtils.extractUserId(new NoUserDetails()));
    }
}
