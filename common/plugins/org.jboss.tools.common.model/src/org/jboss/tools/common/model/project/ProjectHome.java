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
package org.jboss.tools.common.model.project;

import java.io.File;
import java.util.Properties;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.wst.common.componentcore.ComponentCore;
import org.eclipse.wst.common.componentcore.ModuleCoreNature;
import org.eclipse.wst.common.componentcore.resources.IVirtualComponent;
import org.eclipse.wst.common.componentcore.resources.IVirtualFolder;
import org.jboss.tools.common.model.XModelConstants;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;
import org.w3c.dom.Element;

public class ProjectHome {

	public String getLocation(IProject project) {
		try {
			String location = ((project.getDescription().getLocation() == null) ? 
					project.getLocation().toString() : 
					project.getDescription().getLocation().toString());
			return getLocation(location);
		} catch (CoreException e) {
			ModelPlugin.getPluginLog().logError(e);
			return ""; //$NON-NLS-1$
		}
	}

	public String getLocation(String projectLocation) {
		String location = projectLocation;
		File f = new File(location + XModelObjectConstants.SEPARATOR + IModelNature.PROJECT_TEMP);
		File ep = new File(location + XModelObjectConstants.SEPARATOR + IModelNature.PROJECT_FILE);
		if(ep.isFile()) {
			// to remove temp file 
			if(f.isFile()) f.delete();
			return getLocationFrom_project_File(location, ep);
		}
		if(f.isFile()) return getLocationFrom_temp_File(location, f);
		return ""; //$NON-NLS-1$
	}
	
	private String getLocationFrom_project_File(String location, File ss) {
		Element element = XMLUtil.getElement(ss);
		String path = ""; //$NON-NLS-1$
		if(element.hasAttribute("workspace-home")) { //$NON-NLS-1$
			path = element.getAttribute("workspace-home"); //$NON-NLS-1$
		} else if(element.hasAttribute("WORKSPACE_HOME")) { //$NON-NLS-1$
			path = element.getAttribute("WORKSPACE_HOME"); //$NON-NLS-1$
		}
		String q = (path.equals(".")) ? location : (path.startsWith("./")) ? location + path.substring(1) : path; //$NON-NLS-1$ //$NON-NLS-2$
		return q;
	}
	
	private String getLocationFrom_temp_File(String location, File s) {
		String path = XModelObjectLoaderUtil.getCDATA(XMLUtil.getElement(s));
		String q = (path.equals(".")) ? location : (path.startsWith("./")) ? location + path.substring(1) : path; //$NON-NLS-1$ //$NON-NLS-2$
		s.delete();
		return q;
	}

	////
	
	public static boolean getLocation(IProject project, Properties p) {
		IPath webInfPath = null;
		
		if(ComponentCore.createComponent(project)!=null) {
			webInfPath = getWebInfPath(project);
		}		
		
		if(webInfPath == null) return false;
		
		IFolder webInfFolder = ResourcesPlugin.getWorkspace().getRoot().getFolder(webInfPath);
		
		p.setProperty(XModelConstants.WORKSPACE, webInfFolder.getLocation().toString());
		p.setProperty(XModelConstants.WORKSPACE_OLD, webInfFolder.getLocation().toString());

		return true;
	}

	//Taken from J2EEUtils and modified
	public static IPath getWebInfPath(IProject project) {		
		IVirtualComponent component = ComponentCore.createComponent(project);		
		IVirtualFolder webInfDir = component.getRootFolder().getFolder(new Path("/WEB-INF")); //$NON-NLS-1$
		IPath modulePath = webInfDir.getWorkspaceRelativePath();
		return (!webInfDir.exists()) ? null : modulePath;
	}

	public static IPath getFirstWebContentPath(IProject project) {
		IPath modulePath = null;
		IVirtualComponent vc = ComponentCore.createComponent(project);
		if (vc == null || vc.getRootFolder() == null)
			return null;
		if (ModuleCoreNature.isFlexibleProject(project)) {
			modulePath = vc.getRootFolder().getWorkspaceRelativePath();
		}

		return modulePath;
	}
}
