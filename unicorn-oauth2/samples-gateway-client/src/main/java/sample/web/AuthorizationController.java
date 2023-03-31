/*
 * Copyright 2020-2022 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package sample.web;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClient;
import org.springframework.security.oauth2.client.annotation.RegisteredOAuth2AuthorizedClient;
import org.springframework.security.oauth2.core.OAuth2Error;
import org.springframework.security.oauth2.core.endpoint.OAuth2ParameterNames;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.concurrent.ExecutionException;

import static org.springframework.security.oauth2.client.web.reactive.function.client.ServerOAuth2AuthorizedClientExchangeFilterFunction.clientRegistrationId;
import static org.springframework.security.oauth2.client.web.reactive.function.client.ServerOAuth2AuthorizedClientExchangeFilterFunction.oauth2AuthorizedClient;


/**
 * @author Joe Grandja
 * @since 0.0.1
 */
@Controller
public class AuthorizationController {
    private final WebClient webClient;
    private final String messagesBaseUri;

    public AuthorizationController(WebClient webClient,
                                   @Value("${messages.base-uri}") String messagesBaseUri) {
        this.webClient = webClient;
        this.messagesBaseUri = messagesBaseUri;
    }

    @GetMapping(value = "/authorize", params = "grant_type=authorization_code")
    public String authorizationCodeGrant(Model model,
                                         @RegisteredOAuth2AuthorizedClient("messaging-client-authorization-code")
                                         OAuth2AuthorizedClient authorizedClient) throws ExecutionException, InterruptedException {

        try {
            String[] messages = this.webClient
                    .get()
                    .uri(this.messagesBaseUri)
                    .attributes(oauth2AuthorizedClient(authorizedClient))
                    .retrieve()
                    .bodyToMono(String[].class)
                    .toFuture().get();
            model.addAttribute("messages", messages);
        } catch (InterruptedException | ExecutionException e) {
            throw new RuntimeException(e);
        }

        return "index";
    }

    // '/authorized' is the registered 'redirect_uri' for authorization_code
    @GetMapping(value = "/authorized", params = OAuth2ParameterNames.ERROR)
    public String authorizationFailed(Model model, ServerHttpRequest request) {

        var queryParams = request.getQueryParams().toSingleValueMap();
        String errorCode = queryParams.get(OAuth2ParameterNames.ERROR);
        if (StringUtils.hasText(errorCode)) {
            model.addAttribute("error",
                    new OAuth2Error(
                            errorCode,
                            queryParams.get(OAuth2ParameterNames.ERROR_DESCRIPTION), queryParams.get(OAuth2ParameterNames.ERROR_URI))
            );
        }

        return "index";
    }

    @GetMapping(value = "/authorize", params = "grant_type=client_credentials")
    public String clientCredentialsGrant(Model model) throws ExecutionException, InterruptedException {

        try {
            String[] messages = this.webClient
                    .get()
                    .uri(this.messagesBaseUri)
                    .attributes(clientRegistrationId("messaging-client-client-credentials"))
                    .retrieve()
                    .bodyToMono(String[].class)
                    .toFuture().get();
            model.addAttribute("messages", messages);
        } catch (InterruptedException | ExecutionException e) {
            throw new RuntimeException(e);
        }

        return "index";
    }
}
