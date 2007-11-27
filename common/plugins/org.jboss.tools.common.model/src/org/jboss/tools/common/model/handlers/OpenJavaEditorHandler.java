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
package org.jboss.tools.common.model.handlers;

import java.util.Properties;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.ide.IDE;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class OpenJavaEditorHandler extends AbstractHandler {
	public boolean isEnabled(XModelObject object) {
		return true;
	}

	public void executeHandler(XModelObject object, Properties p) throws Exception {
		IProject project = (IProject)object.getModel().getProperties().get("project");
		IJavaProject javaProject = (IJavaProject)project.getNature(JavaCore.NATURE_ID);
	
		String className = 
			EclipseResourceUtil.getJavaClassQualifiedName(object).replace('.', '/') +
			"." + object.getAttributeValue("extension");
		
		IJavaElement javaElement = javaProject.findElement(new Path(className));
		if(javaElement != null) {
			JavaUI.openInEditor(javaElement);
		} else {
			IFile f = (IFile)EclipseResourceUtil.getResource(object);
			if(f != null) IDE.openEditor(getWorkbenchPage(), f);
		}
	}
	private IWorkbenchPage getWorkbenchPage() {
		ModelPlugin plugin = ModelPlugin.getDefault();
		IWorkbench workbench = (plugin == null) ? null : plugin.getWorkbench();
		IWorkbenchWindow window = (workbench == null) ? null : workbench.getActiveWorkbenchWindow();
		return (window == null) ? null : window.getActivePage();
	}
}
