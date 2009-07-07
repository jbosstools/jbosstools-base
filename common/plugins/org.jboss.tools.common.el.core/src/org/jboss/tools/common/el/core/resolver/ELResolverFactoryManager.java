/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.resolver;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.InvalidRegistryObjectException;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.el.core.Activator;

/**
 * This factory is used to get EL Resources for particular resource
 * which are plug in via org.jboss.tools.common.el.core.elResolver extension point.
 * @author Alexey Kazakov
 */
public class ELResolverFactoryManager {

	private static final ELResolverFactoryManager INSTANCE = new ELResolverFactoryManager();

//	private Map<String, ELResolver[]> resolvers = new HashMap<String, ELResolver[]>();

	private ELResolverFactoryManager() {
	}

	/**
	 * Returns an instance of factory manager
	 * @return
	 */
	public static ELResolverFactoryManager getInstance() {
		return INSTANCE;
	}

	/**
	 * Returns all EL resolvers for the resource
	 * @param resource
	 * @return
	 */
	public ELResolver[] getResolvers(IResource resource) {
		IProject project = resource.getProject();
		if(project==null) {
			return new ELResolver[0];
		}
//		ELResolver[] result =  resolvers.get(project.getName());
//		if(result!=null) {
//			return result;
//		}
		Set<ELResolver> resolverSet = new HashSet<ELResolver>();
        IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = registry.getExtensionPoint("org.jboss.tools.common.el.core.elResolver");  //$NON-NLS-1$
		IExtension[] extensions = extensionPoint.getExtensions();
		for (int i=0; i<extensions.length; i++) {
			IExtension extension = extensions[i];
			IConfigurationElement[] elements = extension.getConfigurationElements();
			for(int j=0; j<elements.length; j++) {
				IConfigurationElement[] natures = elements[j].getChildren("project-nature"); //$NON-NLS-1$
				for (int k = 0; k < natures.length; k++) {
					String natureId = natures[k].getAttribute("id"); //$NON-NLS-1$
					try {
						if(project.hasNature(natureId)) {
							Object resolver = natures[k].createExecutableExtension("resolver-class"); //$NON-NLS-1$
							if(resolver instanceof ELResolver) {
								resolverSet.add((ELResolver)resolver);
							} else {
								Activator.getPluginLog().logError(resolver.getClass().getName() + " must be instance of org.jboss.tools.common.el.core.resolver.ELResolver"); //$NON-NLS-1$
							}
						}
					} catch (InvalidRegistryObjectException e) {
						Activator.getPluginLog().logError(e);
					} catch (CoreException e) {
						Activator.getPluginLog().logError(e);
					}
				}
				IConfigurationElement[] factories = elements[j].getChildren("factory"); //$NON-NLS-1$
				for (int k = 0; k < factories.length; k++) {
					try {
						Object factory = factories[k].createExecutableExtension("class"); //$NON-NLS-1$
						if(factory instanceof ELResolverFactory) {
							ELResolver resolver = ((ELResolverFactory)factory).createResolver(resource);
							if(resolver!=null) {
								resolverSet.add(resolver);
							}
						} else {
							Activator.getPluginLog().logError(factory.getClass().getName() + " must be instance of org.jboss.tools.common.el.core.resolver.ELResolverFactory"); //$NON-NLS-1$
						}
					} catch (CoreException e) {
						Activator.getPluginLog().logError(e);
					}
				}
			}
		}
//		result = resolverSet.toArray(new ELResolver[resolverSet.size()]);
//		resolvers.put(project.getName(), result);
//		return result;
		return resolverSet.toArray(new ELResolver[0]);
	}
}