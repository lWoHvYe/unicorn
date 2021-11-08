package com.lwohvye.modules.system.service.local;

import com.lwohvye.modules.system.service.TerminalService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class WindowsTerminalServiceImpl implements TerminalService {
    @Override
    public void prSysName() {
        log.warn(" This is Windows 11 ");
    }
}
