package com.lwohvye.modules.system.service.local;

import com.lwohvye.modules.system.service.ITerminalService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class MacOSTerminalServiceImpl implements ITerminalService {
    @Override
    public void prSysName() {
        log.warn(" This is Darwin lWoHvYedeMac-mini.local 20.6.0 Darwin Kernel Version 20.6.0 ");
    }
}
