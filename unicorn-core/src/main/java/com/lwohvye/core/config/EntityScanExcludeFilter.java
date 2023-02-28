pachage com.lwohvye.core.config

import org.springframework.beans.factory.aot.BeanRegistrationExcludeFilter;
import org.springframework.beans.factory.support.RegisteredBean;
import org.springframework.boot.autoconfigure.domain.EntityScanPackages;

class EntityScanExcludeFilter implements BeanRegistrationExcludeFilter {

	@Override
	public boolean isExcludedFromAotProcessing(RegisteredBean registeredBean) {
		return registeredBean.getBeanClass().equals(EntityScanPackages.class);
	}

}
