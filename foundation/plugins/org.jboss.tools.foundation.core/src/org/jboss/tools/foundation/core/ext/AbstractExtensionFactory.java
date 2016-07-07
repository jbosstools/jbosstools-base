/*******************************************************************************
 * Copyright (c) 2016 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.core.ext;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IExecutableExtensionFactory;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.e4.core.contexts.ContextInjectionFactory;
import org.eclipse.e4.core.contexts.IEclipseContext;
import org.osgi.framework.Bundle;

/**
 * Base workbench extension factory. 
 * 
 * By default, extensions are created using the {@link ContextInjectionFactory}, 
 * and are re-injected into the same {@link IEclipseContext} they were created from.
 */
public abstract class AbstractExtensionFactory implements IExecutableExtension, IExecutableExtensionFactory {
	
	protected IConfigurationElement config;
	protected String data;
	protected String propertyName;

	@Override
	public Object create() throws CoreException {
		try {
			return configureInstance(getInstance());
		}
		catch (Exception e) {
			throw new CoreException(new Status(IStatus.ERROR, getBundle().getSymbolicName(), e.getMessage() + 
					" AbstractExtensionFactory: "+ getClass().getName(), e));
		}
	}
	
	private Object configureInstance(Object instance) throws CoreException {
		if (instance instanceof IExecutableExtension) {
			((IExecutableExtension) instance).setInitializationData(config, propertyName, null);
		}
		return instance;
	}
	
	@Override
	public void setInitializationData(IConfigurationElement config, String propertyName, Object data)
		throws CoreException {
		if (!(data instanceof String)) {
			throw new IllegalArgumentException("Data argument must be a String for " + getClass()); //$NON-NLS-1$
		}
		this.data = (String) data;
		this.propertyName = propertyName;
		this.config = config;
	}
	
	/**
	 * Creates the extension from an IEclipseContext and injects it into the context.
	 * 
	 * @return the extension instance.
	 */
	protected Object getInstance() throws Exception {
		Class<?> clazz = getBundle().loadClass(data);
		IEclipseContext ctx = getContext();
		Object result = ContextInjectionFactory.make(clazz, ctx);
		ctx.set(clazz.toString(), result);
		return result;
	}
	
	/**
	 * @return the bundle used for loading the extension.
	 */
	protected abstract Bundle getBundle();
	
	/**
	 * @return the context to create and inject the extension from/to.
	 */
	protected abstract IEclipseContext getContext();
}