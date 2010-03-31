/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.verification.vrules.core.resources;

import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import org.jboss.tools.common.verification.vrules.plugin.VerificationPlugin;

//This builder is obsolete. It's functionality is moved to WTP's Validation framework.
//The builder is preserved for backward compatibility.
public class VerifyBuilder extends IncrementalProjectBuilder
{
	public static final String BUILDER_ID = VerificationPlugin.PLUGIN_ID + ".verifybuilder"; //$NON-NLS-1$
//	GlobalBuilderImpl builderImpl = null;
	
	protected IProject[] build(int kind, Map args, IProgressMonitor monitor)
		throws CoreException
	{
//		IProject currentProject = getProject();
//		IModelNature n = EclipseResourceUtil.getModelNature(currentProject);
//		if (n != null) {
//			if(builderImpl == null) {
//				builderImpl = new GlobalBuilderImpl();
//				builderImpl.setModel(n.getModel());
//			}
//			builderImpl.execute(null);
//		}
		
		return null;
	}
}
