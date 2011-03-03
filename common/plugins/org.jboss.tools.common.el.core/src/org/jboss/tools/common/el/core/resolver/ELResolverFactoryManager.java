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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.el.core.ELCorePlugin;

/**
 * This factory is used to get EL Resources for particular resource
 * which are plug in via org.jboss.tools.common.el.core.elResolver extension point.
 * @author Alexey Kazakov
 */
public class ELResolverFactoryManager {

	private static final ELResolverFactoryManager INSTANCE = new ELResolverFactoryManager();

	private static HashMap<String, Set<ELResolver>> resolversByNature;
	private static Set<ELResolverFactory> resolverFactories;

	private ELResolverFactoryManager() {
	}

	/**
	 * Returns an instance of factory manager
	 * @return
	 */
	public static ELResolverFactoryManager getInstance() {
		return INSTANCE;
	}

	private void init() {
		if(resolversByNature==null) {
			resolversByNature = new HashMap<String, Set<ELResolver>>();
			resolverFactories = new HashSet<ELResolverFactory>();

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
							Object resolver = natures[k].createExecutableExtension("resolver-class"); //$NON-NLS-1$
							if(resolver instanceof ELResolver) {
								Set<ELResolver> resolverSet = resolversByNature.get(natureId);
								if(resolverSet==null) {
									resolverSet = new HashSet<ELResolver>();
									resolversByNature.put(natureId, resolverSet);
								}
								resolverSet.add((ELResolver)resolver);
							} else {
								ELCorePlugin.getPluginLog().logError(resolver.getClass().getName() + " must be instance of org.jboss.tools.common.el.core.resolver.ELResolver"); //$NON-NLS-1$
							}
						} catch (CoreException e) {
							ELCorePlugin.getPluginLog().logError(e);
						}
					}
					IConfigurationElement[] factories = elements[j].getChildren("factory"); //$NON-NLS-1$
					for (int k = 0; k < factories.length; k++) {
						try {
							Object factory = factories[k].createExecutableExtension("class"); //$NON-NLS-1$
							if(factory instanceof ELResolverFactory) {
								resolverFactories.add((ELResolverFactory)factory);
							} else if(!(factory instanceof TestELResolverFactory)) {
								ELCorePlugin.getPluginLog().logError(factory.getClass().getName() + " must be instance of org.jboss.tools.common.el.core.resolver.ELResolverFactory"); //$NON-NLS-1$
							}
						} catch (CoreException e) {
							ELCorePlugin.getPluginLog().logError(e);
						}
					}
				}
			}
		}
	}

	/**
	 * Returns all EL resolvers for the resource
	 * @param resource
	 * @return
	 */
	public ELResolver[] getResolvers(IResource resource) {
		if(!resource.isAccessible()) {
			return new ELResolver[0];
		}
		IProject project = resource.getProject();
		if (project == null || !project.isAccessible()) {
			return new ELResolver[0];
		}

		init();

		Set<ELResolver> resolverSet = new HashSet<ELResolver>();
		Set<String> ids = resolversByNature.keySet();
		for (String natureId : ids) {
			try {
				if(project.hasNature(natureId)) {
					resolverSet.addAll(resolversByNature.get(natureId));
				}
			} catch (CoreException e) {
				ELCorePlugin.getPluginLog().logError(e);
			}
		}
		for (ELResolverFactory factory : resolverFactories) {
			ELResolver resolver = factory.createResolver(resource);
			if(resolver!=null) {
				resolverSet.add(resolver);
			}
		}
		return resolverSet.toArray(new ELResolver[resolverSet.size()]);
	}
}