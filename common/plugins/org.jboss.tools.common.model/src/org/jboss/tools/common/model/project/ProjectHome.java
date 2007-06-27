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
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;

public class ProjectHome {

	public String getLocation(IProject project) {
		try {
			String location = ((project.getDescription().getLocation() == null) ? 
					project.getLocation().toString() : 
					project.getDescription().getLocation().toString());
			return getLocation(location);
		} catch (CoreException e) {
			ModelPlugin.getPluginLog().logError(e);
			return "";
		}
	}

	public String getLocation(String projectLocation) {
		String location = projectLocation;
		File f = new File(location + "/" + IModelNature.PROJECT_TEMP);
		File ep = new File(location + "/" + IModelNature.PROJECT_FILE);
		if(ep.isFile()) {
			// to remove .struts file if workspace.pex is not found
			if(f.isFile()) checkOldStrutsAndPexFile(location, f);
			return getLocationFrom_strutsstudio_File(location, ep);
		}
		if(f.isFile()) return getLocationFrom_struts_File(location, f);
		return "";
	}
	
	private String getLocationFrom_strutsstudio_File(String location, File ss) {
		String path = XMLUtil.getElement(ss).getAttribute("WORKSPACE_HOME");
		String q = (path.equals(".")) ? location : (path.startsWith("./")) ? location + path.substring(1) : path;
		return q;
	}
	
	private void checkOldStrutsAndPexFile(String location, File s) {
		String path = XModelObjectLoaderUtil.getCDATA(XMLUtil.getElement(s));
		String q = (path.startsWith("./")) ? location + path.substring(1) : path;
		File pex = new File(q + "/workspace.pex");
		if(!pex.exists()) s.delete();
	}

	private String getLocationFrom_struts_File(String location, File s) {
		String path = XModelObjectLoaderUtil.getCDATA(XMLUtil.getElement(s));
		String q = (path.equals(".")) ? location : (path.startsWith("./")) ? location + path.substring(1) : path;
		s.delete();
		return q;
	}

}
