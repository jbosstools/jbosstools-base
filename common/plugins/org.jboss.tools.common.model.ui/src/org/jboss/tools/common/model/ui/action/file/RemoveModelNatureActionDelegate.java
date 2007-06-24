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
package org.jboss.tools.common.model.ui.action.file;

import java.util.*;
import org.eclipse.core.resources.IProject;

public class RemoveModelNatureActionDelegate extends ProjectRootActionDelegate {
	protected String getActionPath() {
		return "EclipseActions.RemoveModelNature";
	}
	
	protected void initProperties(Properties p) {
		String nature = getModelNatureName(); 
		if(nature != null) p.setProperty("nature", nature);
	}
	
	protected boolean hasModelNature(IProject project) {
		String nature = getModelNatureName();
		if(nature == null) return super.hasModelNature(project); 
		try {
			if(project.hasNature(nature)) return true;
		} catch (Exception e) {
		}
		return false;
	}
	protected String getModelNatureName() {
		return null;
	}
	
}
