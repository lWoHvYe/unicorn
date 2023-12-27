package sample.config;

import feign.Logger;
import feign.Request;
import feign.Response;
import feign.Util;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;

import static feign.Logger.Level.HEADERS;

@Slf4j
public class CustomFeignRequestLogging extends Logger {

    @Override
    protected void logRequest(String configKey, Level logLevel, Request request) {

        if (logLevel.ordinal() >= HEADERS.ordinal()) {
            super.logRequest(configKey, logLevel, request);
        } else {
            var bodyText = request.charset() != null ? new String(request.body(), request.charset()) : null;
            log(configKey, "---> %s %s HTTP/1.1 (%s body) ", request.httpMethod().name(), request.url(), bodyText);
        }
    }

    @Override
    protected Response logAndRebufferResponse(String configKey, Level logLevel, Response response, long elapsedTime)
            throws IOException {
        if (logLevel.ordinal() >= HEADERS.ordinal()) {
            return super.logAndRebufferResponse(configKey, logLevel, response, elapsedTime);
        } else {
            int status = response.status();
            Request request = response.request();
            byte[] bodyData = Util.toByteArray(response.body().asInputStream());
            log(configKey, "<--- %s %s HTTP/1.1 %s (response %s) (%sms) ", request.httpMethod().name(), request.url(),
                    status, Util.decodeOrDefault(bodyData, Util.UTF_8, "Binary data"), elapsedTime);
            return response.toBuilder().body(bodyData).build();
        }
    }


    @Override
    protected void log(String configKey, String format, Object... args) {
        log.info(format(configKey, format, args));
    }

    protected String format(String configKey, String format, Object... args) {
        return String.format(methodTag(configKey) + format, args);
    }
}
