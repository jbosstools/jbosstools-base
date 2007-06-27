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
import java.util.*;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.w3c.dom.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.loaders.impl.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.project.WatcherLoader;

public class FileSystemsLoader extends URLRootLoader {
	public static String IS_ADDED_TO_CLASSPATH = "isAddedToClassPath";
	
	FileSystemsLoaderUtil fsutil = new FileSystemsLoaderUtil();

    public FileSystemsLoader() {}

    protected XModelObjectLoaderUtil util() {
        return fsutil;
    }

    public boolean save(XModelObject object) {
        if(!fsutil.isModified(object)) return true;
        String s = getEclipseFileName(object, false);
        boolean b = (s == null) ? super.save(object.copy(1)) :
           saveEclipse(object.copy(1));
        if(((FileSystemsImpl)object).requestSave()) return true;
        XModelObject[] cs = object.getChildren();
        for (int i = 0; i < cs.length; i++) {
            XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(cs[i]);
            if(loader != null) b &= loader.save(cs[i]);
        }
        return b;
    }
    
    public boolean update(XModelObject object) {
        boolean b = true;
        XModelObject[] cs = object.getChildren();
        for (int i = 0; i < cs.length; i++) {
            XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(cs[i]);
            if(loader != null) b &= loader.update(cs[i]);
        }
		updateLibs(object);
		removeMissingJarSystems(object);
        try {
        	((FileSystemsImpl)object).updateOverlapped();
        } catch (Exception e) {
        	ModelPlugin.getPluginLog().logError(e);
        }
        return b;
    }

	public void load(XModelObject object) {
		if(EclipseResourceUtil.isProjectFragment(object.getModel())) return;
		String f = getEclipseFileName(object, true);
		if(f == null) super.load(object);
		else util().load(new File(f), object);
		removeMissingJarSystems(object);
	}
	
	void removeMissingJarSystems(XModelObject object) {
		XModelObject[] os = object.getChildren("FileSystemJar");
		for (int i = 0; i < os.length; i++) {
			JarSystemImpl jar = (JarSystemImpl)os[i];
			String location = jar.getLocation();
			if(location != null && !new File(location).isFile()) {
				jar.removeFromParent();
				object.setModified(true);
			}
		}
	}

    public String fileroot(XModelObject object) {
        return XModelConstants.getWorkspace(object.getModel()) + "/";
    }

    protected String fileName(XModelObject object) {
        return XModelConstants.getProjectPrefix(object.getModel()) + "workspace.pex";
    }
    
    private boolean saveEclipse(XModelObject object) {
    	String f = getEclipseFileName(object, false);
    	boolean b = f != null && util().save(new File(f), object);
    	if(b) {
    		IFile file = EclipseResourceUtil.getFile(f);
    		if(file != null) {
    			try {
    				file.refreshLocal(0, null);
    			} catch (Exception e) {
    				// ignore
    			}
    		}
    	}
    	return b;
    }
    
    private String getEclipseFileName(XModelObject object, boolean load) {
		String project = object.getModel().getProperties().getProperty(IModelNature.ECLIPSE_PROJECT);
		if(project == null) return null;
		String fn = project + "/" + IModelNature.PROJECT_FILE;
		if(!load || new File(fn).exists()) return fn;
		return null;
    }
    
    private void updateLibs(XModelObject object) {
    	if(WatcherLoader.isLocked(object.getModel())) {
    		return;
    	}
    	IProject project = (IProject)object.getModel().getProperties().get("project");
    	if(project == null) return;
    	XModelObject lib = validateLib(object);
    	if(lib == null) {
    		return;
    	}
    	XModelObject[] js = lib.getChildren();
    	String loc = lib.getAttributeValue("location");
		List paths = null;
		try {
			paths = EclipseResourceUtil.getClassPath(project);
		} catch (Exception e) {
			//ignore
		}
		if(paths == null) return;
    	for (int i = 0; i < js.length; i++) {
    		if(js[i].getFileType() != XModelObject.FILE) continue;
    		String nm = js[i].getPathPart();
    		String jsname = "lib-" + nm;
			String location = loc + "/" + nm;
			String path = XModelObjectUtil.expand(location, object.getModel(), null);
			try {
				path = new File(path).getCanonicalPath();
			} catch (Exception e) {
				//ignore
			}
    		XModelObject s = object.getChildByPath(jsname);
    		if(s != null) {
				if(!paths.contains(path)) {
					if("true".equals(s.get(IS_ADDED_TO_CLASSPATH))) {
						s.removeFromParent(); 
						object.setModified(true);
					} else if(!new File(path).exists()) {
						s.removeFromParent();
						object.setModified(true);
					}
				} else {
					s.set(IS_ADDED_TO_CLASSPATH, "true");
				} 
    		} else {
				if(paths.contains(path)) {
					s = object.getModel().createModelObject("FileSystemJar", null);
					s.setAttributeValue("name", jsname);
					s.setAttributeValue("location", location);
					s.set(IS_ADDED_TO_CLASSPATH, "true");
					object.addChild(s);
					object.setModified(true);
				}
    		}
    	}
    }
    
    private XModelObject validateLib(XModelObject object) {
    	XModelObject lib = object.getChildByPath("lib");
    	if(lib == null) {
    		XModelObject wi = object.getChildByPath("WEB-INF");
    		if(wi == null) return null;
    		XModelObject lb = wi.getChildByPath("lib");
    		if(lb == null) return null;
    		lib = wi.getModel().createModelObject("FileSystemFolder", null);
    		lib.setAttributeValue("name", "lib");
    		lib.setAttributeValue("location", wi.getAttributeValue("location") + "/lib");
    		object.addChild(lib);
    		object.setModified(true);
    	}
    	return lib;
    }
}

class FileSystemsLoaderUtil extends XModelObjectLoaderUtil {
    public boolean saveChildren(Element element, XModelObject o) {
        boolean b = super.saveChildren(element, o);
        if(b && "FILESYSTEMS".equals(element.getNodeName())) {
        	saveWorkspaceHomeAttr(element, o);
            XModelObject w = getWeb(o);
            if(w != null) save(element, w);
        }
        return b;
    }

    public void loadChildren(Element element, XModelObject o) {
        super.loadChildren(element, o);
        if("FILESYSTEMS".equals(element.getNodeName())) {
            Element e = XMLUtil.getUniqueChild(element, "WEB");
            XModelObject w = getWeb(o);
            if(w != null && e != null) load(e, w);
        }
    }

	boolean isModified(XModelObject object) {
		if(object.isModified()) return true;
		XModelObject w = getWeb(object);
		return w != null && w.isModified();    	 
	}
	
	private XModelObject getWeb(XModelObject object) {
		return object.getModel().getByPath("Web");
	}
	
	private void saveWorkspaceHomeAttr(Element element, XModelObject o) {
		Properties p = o.getModel().getProperties();
		String project = p.getProperty(IModelNature.ECLIPSE_PROJECT);
		String workspace = p.getProperty("redhat.workspace");
		if(project == null) return;
		String relative = workspace.startsWith(project + "/") ? 
		    "." + workspace.substring(project.length()) : workspace;
		element.setAttribute("WORKSPACE_HOME", relative);
	}

}

