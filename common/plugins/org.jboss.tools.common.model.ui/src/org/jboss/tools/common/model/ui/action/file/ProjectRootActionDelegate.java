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

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.IJavaProject;

public class ProjectRootActionDelegate extends ModelResourceActionDelegate {
	protected boolean isSupportingImplementation(Class cls) {
		return (cls == IProject.class || cls == IJavaProject.class);
	}	

}
