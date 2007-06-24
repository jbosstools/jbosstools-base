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
package org.jboss.tools.common.model.filesystems.impl;

import java.io.File;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.filesystems.*;

public class FileSystemImpl extends FolderImpl implements FileSystem {
    private static final long serialVersionUID = 7212433789685679616L;
    protected FileSystemPeer peer = new FileSystemPeer();
    protected IProject project = null;

    public FileSystemImpl() {}

    public int getFileType() {
        return SYSTEM;
    }

    protected FileSystemImpl getFileSystem() {
		if (getProject() != null) getResource();
        return this;
    }

    public IProject getProject() {
    	if(project == null) {
    		project = (IProject)getModel().getProperties().get("project");
    	}  	
    	return project;
    }

    public IContainer getResource() {
    	if(getProject() == null || resource != null) return resource;
    	if(!project.isOpen() || project.getLocation() == null) return resource;
    	String prloc = project.getLocation().toString().replace('\\', '/');
    	String thloc = XModelObjectUtil.getExpandedValue(this, "location", null);
    	try {
    		prloc = new File(prloc).getCanonicalPath().replace('\\','/');
    	} catch (Exception e) {
    		//ignore
    	}
    	try {
    		java.io.File f = new java.io.File(thloc);
    		if(f.exists() || thloc.indexOf("/..") >= 0) {
    			thloc = f.getCanonicalPath().replace('\\', '/');
    		} else {
    			thloc = thloc.replace('\\', '/');
    		}
    	} catch (Exception e) {
    		return null;
    	}
		if(thloc.equalsIgnoreCase(prloc)) return resource = project; ///
		if(prloc.toLowerCase().startsWith(thloc.toLowerCase())) return resource = null;
		if(thloc.toLowerCase().startsWith(prloc.toLowerCase())) {
			String relative = thloc.substring(prloc.length());
			try {
				IFolder f = project.getFolder(new Path(relative));
				if(!f.exists()) {
					try {
						if(f.getParent() != null && f.getParent().exists()) {
							f.create(true, true, null);
						}
					} catch (Exception e) {
						ModelPlugin.log(e);
					}
				}
				return resource = f;			
			} catch (Exception e) {
				ModelPlugin.log(e);
			}
		}

    	IFolder f = project.getFolder(new Path("/" + getAttributeValue("name")));
    	if(!f.exists()) {
    		try {
				f.createLink(new Path(thloc), IFolder.FORCE, null);
				resource = f;
    		} catch (Exception e) {
    			ModelPlugin.log("Cannot create link: " + e.getMessage());
    			ModelPlugin.log("Project path=" + prloc);
    			ModelPlugin.log("   Link path=" + thloc);
    		}    		
    	} else resource = f;
    	return resource;
    }
    
	protected boolean needUpdateResource() {
		return false;
	}

    public String getAbsoluteLocation() {
		String s = XModelObjectUtil.getExpandedValue(this, "location", null);
		try {
			return new java.io.File(s).getCanonicalPath();
		} catch (Exception e) {
			//ignore if file does not exist, just use its path
			return s;
		}
    }

    public boolean isAttributeEditable(String name) {
        return false;
    }

    protected String getAbsolutePath() {
        return "" + get("LOCATION");
    }

    public FileSystemPeer getPeer() {
        return peer;
    }

    public String getPathPart() {
        return name();
    }

    public String getTempLocation() {
        return getAbsolutePath();
    }

    public LFileObject getFileObject(String relpath) {
        return new LFileObjectImpl(getAbsolutePath(), name(), relpath);
    }

    public String getMainIconName() {
        String info = getAttributeValue("info");
        if(info == null || info.length() == 0) return super.getMainIconName();
        int mr = info.indexOf("Struts-Module=/");
        if(mr >= 0) return "main.struts.moduleroot";
        int wc = info.indexOf("Content-Type=Web");
        if(wc >= 0) return "main.webprj.webroot";
        return super.getMainIconName();
    }

    public String getPresentationString() {
    	IResource r = getResource();
    	String resourceName = (r == null) ? null : r.getName();
    	String natureProperty = getModel().getProperties().getProperty("nature");
		if("org.jboss.tools.jsf.jsfnature".equals(natureProperty)) {
			return resourceName;
		}
    	String p = super.getPresentationString();
    	if(p == null || p.equals(resourceName)) {
    		if(resourceName != null && resource.isLinked()) return resourceName.replace('#', '/');
    		return resourceName;
    	}
    	return p.replace('#', '/') + " (" + resourceName + ")";
    }
}