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
import java.io.IOException;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.jboss.tools.common.model.XModelObjectConstants;
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
    		project = EclipseResourceUtil.getProject(this);
    	}  	
    	return project;
    }

    public IContainer getResource() {
    	if(getProject() == null || resource != null) return resource;
    	if(!project.isOpen() || project.getLocation() == null) return resource;
    	String prloc = project.getLocation().toString().replace('\\', '/');
    	String thloc = XModelObjectUtil.getExpandedValue(this, XModelObjectConstants.ATTR_NAME_LOCATION, null);
    	try {
    		prloc = new File(prloc).getCanonicalPath().replace('\\','/');
    	} catch (IOException e) {
    		//ignore
    	}
    	try {
    		java.io.File f = new java.io.File(thloc);
    		if(f.exists() || thloc.indexOf("/..") >= 0) { //$NON-NLS-1$
    			thloc = f.getCanonicalPath().replace('\\', '/');
    		} else {
    			thloc = thloc.replace('\\', '/');
    		}
    	} catch (IOException e) {
    		return null;
    	}
		if(thloc.equalsIgnoreCase(prloc)) return resource = project; ///
		if(prloc.toLowerCase().startsWith(thloc.toLowerCase())) return resource = null;
		if(thloc.toLowerCase().startsWith(prloc.toLowerCase())) {
			String relative = thloc.substring(prloc.length());
			IFolder f = project.getFolder(new Path(relative));
			if(!f.exists() && !f.isSynchronized(IResource.DEPTH_ONE)) {
				try {
					f.refreshLocal(IResource.DEPTH_ONE, new NullProgressMonitor());
				} catch (CoreException e) {
					ModelPlugin.getPluginLog().logError(e);
				}
			}
			if(!f.exists()) {
				try {
					if(f.getParent() != null && f.getParent().exists()) {
						f.create(true, true, null);
					}
				} catch (CoreException e) {
					ModelPlugin.getPluginLog().logError(e);
				}
			}
			return resource = f;			
		}

    	IFolder f = project.getFolder(new Path(XModelObjectConstants.SEPARATOR + getAttributeValue(XModelObjectConstants.ATTR_NAME)));
    	if(!f.exists()) {
    		try {
				f.createLink(new Path(thloc), IFolder.FORCE, null);
				resource = f;
    		} catch (CoreException e) {
    			ModelPlugin.getPluginLog().logError("Cannot create link: " + e.getMessage()); //$NON-NLS-1$
    			ModelPlugin.getPluginLog().logError("Project path=" + prloc); //$NON-NLS-1$
    			ModelPlugin.getPluginLog().logError("   Link path=" + thloc); //$NON-NLS-1$
    		}    		
    	} else resource = f;
    	return resource;
    }
    
	protected boolean needUpdateResource() {
		return false;
	}

    public String getAbsoluteLocation() {
		String s = XModelObjectUtil.getExpandedValue(this, XModelObjectConstants.ATTR_NAME_LOCATION, null);
		if(s == null || s.length() == 0) return s;
		try {
			return new java.io.File(s).getCanonicalPath();
		} catch (IOException e) {
			//ignore if file does not exist, just use its path
			return s;
		}
    }

    public boolean isAttributeEditable(String name) {
        return false;
    }

    protected String getAbsolutePath() {
        return "" + get(XModelObjectConstants.ATTR_NAME_LOCATION); //$NON-NLS-1$
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
        String info = getAttributeValue("info"); //$NON-NLS-1$
        if(info == null || info.length() == 0) return super.getMainIconName();
        int mr = info.indexOf("Struts-Module=/"); //$NON-NLS-1$
        if(mr >= 0) return "main.struts.moduleroot"; //$NON-NLS-1$
        int wc = info.indexOf("Content-Type=Web"); //$NON-NLS-1$
        if(wc >= 0) return "main.webprj.webroot"; //$NON-NLS-1$
        return super.getMainIconName();
    }

    public String getPresentationString() {
    	IResource r = getResource();
    	String resourceName = (r == null) ? null : r.getName();
    	String natureProperty = getModel().getProperties().getProperty("nature"); //$NON-NLS-1$
		if("org.jboss.tools.jsf.jsfnature".equals(natureProperty)) { //$NON-NLS-1$
			return resourceName;
		}
    	String p = super.getPresentationString();
    	if(p == null || p.equals(resourceName)) {
    		if(resourceName != null && resource.isLinked()) return resourceName.replace('#', '/');
    		return resourceName;
    	}
    	if(XModelObjectConstants.TRUE.equals(getModel().getProperties().getProperty("isProjectFragment"))) { //$NON-NLS-1$
    		return resourceName;
    	}
    	return p.replace('#', '/') + " (" + resourceName + ")"; //$NON-NLS-1$ //$NON-NLS-2$
    }
}