/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.model.ui.internal.handlers;

import java.util.*;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class RemoveModelNatureHandler extends ProjectRootHandler {
	public static String PARAM_CONTRIBUTION = org.jboss.tools.common.model.handlers.RemoveModelNatureHandler.PARAM_CONTRIBUTION;
	
	protected String getActionPath() {
		return "EclipseActions.RemoveModelNature"; //$NON-NLS-1$
	}
	
	protected void initProperties(Properties p) {
		String nature = getModelNatureName(); 
		if(nature != null) p.setProperty("nature", nature); //$NON-NLS-1$
	}
	
	protected boolean hasModelNature(IProject project) {
		String nature = getModelNatureName();
		if(nature == null) return super.hasModelNature(project);
		if(project == null || !project.isAccessible()) return false;
		try {
			if(project.hasNature(nature)) return true;
		} catch (CoreException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		return false;
	}
	protected String getModelNatureName() {
		return null;
	}
	
}
