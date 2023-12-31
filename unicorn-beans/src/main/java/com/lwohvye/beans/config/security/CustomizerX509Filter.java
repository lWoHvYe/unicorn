/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.beans.config.security;

import jakarta.servlet.FilterChain;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.bouncycastle.asn1.x500.style.BCStyle;
import org.bouncycastle.cert.jcajce.JcaX509CertificateHolder;
import org.springframework.web.filter.OncePerRequestFilter;

import java.security.cert.X509Certificate;
import java.util.Objects;

@Slf4j
@RequiredArgsConstructor
public class CustomizerX509Filter extends OncePerRequestFilter {

    @SneakyThrows
    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) {
        var certs = (X509Certificate[]) request.getAttribute("javax.servlet.request.X509Certificate");
        if (Objects.nonNull(certs) && certs.length > 0) {
            var clientCert = certs[0];
            var x500Name = new JcaX509CertificateHolder(clientCert).getSubject();
            var cn = x500Name.getRDNs(BCStyle.CN)[0];
            var cnName = cn.getFirst().getValue().toString();
            log.info("CN retract success {}", cnName);
        } else
            log.info("cert is null, byPass");
        filterChain.doFilter(request, response);
    }
}
