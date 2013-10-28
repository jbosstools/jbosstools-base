/*******************************************************************************
  * Copyright (c) 2010 - 2013 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/

package org.jboss.tools.common.el.core.test.resolver;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.jboss.tools.common.el.core.resolver.ELResolver;
import org.jboss.tools.common.el.core.resolver.ELResolverFactory;

public class ELResolverFactory1 implements ELResolverFactory {

	public ELResolverFactory1() {
	}

	public ELResolver createResolver(IResource resource) {
		IProject project = resource == null ? null : resource.getProject();
		try {
			return (project != null && project.hasNature(ProjectNature1.ID) ? 
					new ResolverProjectNature1() : null);
		} catch (CoreException e) {
			return null;
		}
	}
}
