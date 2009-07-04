/*************************************************************************************
 * Copyright (c) 2008-2009 JBoss by Red Hat and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.verification.ui.actions;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.wst.common.project.facet.core.FacetedProjectFramework;
import org.jboss.tools.common.verification.ui.XStudioVerificationPlugin;

/**
* @author snjeza
* 
*/
public class ResourceExcludedExpressionFactoryTester extends PropertyTester {

	public static final String RESOURCE_EXCLUDED_EXPRESSION_PROPERTY = "resourceExcluded"; //$NON-NLS-1$
	public static final String RESOURCE_INCLUDED_EXPRESSION_PROPERTY = "resourceIncluded"; //$NON-NLS-1$
	/* (non-Javadoc)
	 * @see org.eclipse.core.expressions.PropertyTester#test(java.lang.Object, java.lang.String, java.lang.Object[], java.lang.Object)
	 */
	public boolean test(Object element, String property, Object[] args, Object expectedValue) {
		if ( !(element instanceof IAdaptable)) {
			return false;
		}
		IResource resource = (IResource) ((IAdaptable) element).getAdapter(IResource.class);
		if (resource == null) {
			return false;
		}
		if (resource instanceof IProject) {
			return false;
		}
		IProject project = resource.getProject();
		if (project == null || !project.exists()) {
			return false;
		}
		Object persistentProperty = null;
		try {
			if (!FacetedProjectFramework.isFacetedProject(project)) {
				return false;
			}
			persistentProperty = resource.getPersistentProperty(XStudioVerificationPlugin.RESOURCE_EXCLUDED);
		} catch (CoreException e) {
			return false;
		}
		if (RESOURCE_EXCLUDED_EXPRESSION_PROPERTY.equals(property)){
	        return persistentProperty != null;
		}
		if (RESOURCE_INCLUDED_EXPRESSION_PROPERTY.equals(property)){
	        return persistentProperty == null;
		}
		return false;
	}
}

