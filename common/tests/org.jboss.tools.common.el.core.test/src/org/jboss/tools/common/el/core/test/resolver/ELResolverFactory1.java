package org.jboss.tools.common.el.core.test.resolver;

import org.eclipse.core.resources.IResource;
import org.jboss.tools.common.el.core.resolver.ELResolver;
import org.jboss.tools.common.el.core.resolver.ELResolverFactory;

public class ELResolverFactory1 implements ELResolverFactory {

	public ELResolverFactory1() {
	}

	public ELResolver createResolver(IResource resource) {
		return new ResolverProjectNature1();
	}

}
