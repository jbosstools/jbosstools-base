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
import java.util.*;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.w3c.dom.*;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.loaders.impl.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.project.IAutoLoad;
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

        if(object.getModel().getProperties().get(XModelConstants.AUTOLOAD) != null) {
        	return true;
        }

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
    
    public boolean update(XModelObject object) throws XModelException {
        boolean b = true;
        XModelObject[] cs = object.getChildren();
        for (int i = 0; i < cs.length; i++) {
            XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(cs[i]);
            if(loader != null) b &= loader.update(cs[i]);
        }
		updateLibs(object);
		removeMissingJarSystems(object);
		updateSrcs(object);
       	((FileSystemsImpl)object).updateOverlapped();
        return b;
    }

	public void load(XModelObject object) {
		if(EclipseResourceUtil.isProjectFragment(object.getModel())) return;

		IAutoLoad auto = (IAutoLoad)object.getModel().getProperties().get(XModelConstants.AUTOLOAD);
        if(auto != null) {
        	auto.load(object.getModel());
       		updateLibs(object);
    		_updateSrcs(object);
           	((FileSystemsImpl)object).updateOverlapped();
        	return;
        }
		
		String f = getEclipseFileName(object, true);
		if(f == null) super.load(object);
		else util().load(new File(f), object);
		
		XModelObject[] os = object.getChildren();
		for (int i = 0; i < os.length; i++) {
			String s = os[i].getAttributeValue("location");
			if(s == null || !s.startsWith(XModelConstants.WORKSPACE_OLD_REF)) continue;
			s = XModelConstants.WORKSPACE_REF + s.substring(XModelConstants.WORKSPACE_OLD_REF.length());
			os[i].setAttributeValue("location", s);
//			System.out.println("Migrated " + s);
		}
		
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
        return "workspace.pex";
    }
    
    private boolean saveEclipse(XModelObject object) {
    	String f = getEclipseFileName(object, false);
    	boolean b = f != null && util().save(new File(f), object);
    	if(b) {
    		IFile file = EclipseResourceUtil.getFile(f);
    		if(file != null) {
    			try {
    				file.refreshLocal(0, null);
    			} catch (CoreException e) {
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
    
    List<String> paths = null;
    
	static String[] SYSTEM_JARS = {"rt.jar", "jsse.jar", "jce.jar", "charsets.jar"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	static Set<String> SYSTEM_JAR_SET = new HashSet<String>();
	
	static {
		for (int i = 0; i < SYSTEM_JARS.length; i++) SYSTEM_JAR_SET.add(SYSTEM_JARS[i]);
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
		List<String> newPaths = null;
		try {
			newPaths = EclipseResourceUtil.getClassPath(project.getProject());
			List<String> jre = EclipseResourceUtil.getJREClassPath(project.getProject());
			if(jre != null) newPaths.removeAll(jre);
		} catch (CoreException e) {
			//TODO
			ModelPlugin.getDefault().logError(e);
		} catch (IOException e) {
			ModelPlugin.getDefault().logError(e);
		}
		if(paths == null && newPaths == null) return;
		if((newPaths == null || paths == null) || (paths.size() != newPaths.size())) {
			paths = newPaths;
		} else { 
			boolean b = false;
			for (int i = 0; i < paths.size() && !b; i++) {
				if(!paths.get(i).equals(newPaths.get(i))) b = true;
			}
			if(!b) return;
			paths = newPaths;
		}
		XModelObject[] fs = object.getChildren("FileSystemJar"); //$NON-NLS-1$
		Set<XModelObject> fss = new HashSet<XModelObject>();
		for (int i = 0; i < fs.length; i++) fss.add(fs[i]);
		
		for (int i = 0; i < paths.size(); i++) {
			String path = paths.get(i);
			if(!EclipseResourceUtil.isJar(path)) continue;
			String fileName = new File(path).getName();
			if(SYSTEM_JAR_SET.contains(fileName)) continue;
			String jsname = "lib-" + fileName; //$NON-NLS-1$
			XModelObject o = object.getChildByPath(jsname); //$NON-NLS-1$
			if(o != null) {
				fss.remove(o);
			} else {
				o = object.getModel().createModelObject("FileSystemJar", null); //$NON-NLS-1$
				o.setAttributeValue("name", jsname); //$NON-NLS-1$
				o.setAttributeValue("location", path); //$NON-NLS-1$
				o.set(IS_ADDED_TO_CLASSPATH, "true"); //$NON-NLS-1$
				object.addChild(o);
//				object.setModified(true);
			}			
		}
		
		for (XModelObject o: fss) {
			String path = XModelObjectUtil.expand(o.getAttributeValue("location"), o.getModel(), null); //$NON-NLS-1$
			if("true".equals(o.get(FileSystemsLoader.IS_ADDED_TO_CLASSPATH))) { //$NON-NLS-1$
				o.removeFromParent(); 
			} else if(!new File(path).exists()) {
				o.removeFromParent();
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

    public void updateSrcs(XModelObject object) {
		IAutoLoad auto = (IAutoLoad)object.getModel().getProperties().get(XModelConstants.AUTOLOAD);
    	if(auto == null && WatcherLoader.isLocked(object.getModel())) {
    		return;
    	}
    	_updateSrcs(object);
    }

    private void _updateSrcs(XModelObject object) {
    	IProject p = EclipseResourceUtil.getProject(object);
    	if(p == null || !p.isAccessible()) return;
		String[] srcs = EclipseResourceUtil.getJavaProjectSrcLocations(p);
		Set<String> paths = new HashSet<String>();
		for (int i = 0; i < srcs.length; i++) {
			String path = EclipseResourceUtil.getRelativeLocation(object.getModel(), srcs[i]);
			if(path == null) continue;
			paths.add(path);
		}
    	XModelObject[] cs = object.getChildren("FileSystemFolder");
    	for (int i = 0; i < cs.length; i++) {
    		if(cs[i].getAttributeValue("name").startsWith("src")) {
    			String loc = cs[i].getAttributeValue("location");
    			if(!paths.contains(loc)) {
    				object.removeChild(cs[i]);
    			} else {
    				paths.remove(loc);
    			}
    		}
    	}
		for (String path : paths) {
			String n = getNextSrcName(object);
			Properties properties = new Properties();
			properties.setProperty("location", path);
			properties.setProperty("name", n);
			FileSystemImpl s = (FileSystemImpl)object.getModel().createModelObject("FileSystemFolder", properties);
			object.addChild(s);
		}
    }

    private String getNextSrcName(XModelObject object) {
    	if(object.getChildByPath("src") == null) return "src";
    	int i = 1;
    	while(true) {
    		String s = "src-" + i;
    		if(object.getChildByPath(s) == null) return s;
    		i++;
    	}
    }

}

class FileSystemsLoaderUtil extends XModelObjectLoaderUtil {
	
	boolean isFileSystems(String nodeName) {
		return "FILESYSTEMS".equals(nodeName) || "file-systems".equals(nodeName);
	}

	protected Set<String> getAllowedChildren(XModelEntity entity) {
		Set<String> children = super.getAllowedChildren(entity);
		if(isFileSystems(entity.getXMLSubPath())) {
			children.add("WEB");
			children.add("web");
		}
		return children;
	}

	protected Set<String> getAllowedAttributes(XModelEntity entity) {
		Set<String> attributes = super.getAllowedAttributes(entity);
		if(isFileSystems(entity.getXMLSubPath())) {
			attributes.add("WORKSPACE_HOME");
			attributes.add("workspace-home");
		}
		return attributes;
	}

	public boolean saveChildren(Element element, XModelObject o) {
        boolean b = super.saveChildren(element, o);
        if(b && isFileSystems(element.getNodeName())) {
        	saveWorkspaceHomeAttr(element, o);
            XModelObject w = getWeb(o);
            if(w != null) save(element, w);
        }
        return b;
    }

    public void loadChildren(Element element, XModelObject o) {
        super.loadChildren(element, o);
        if(isFileSystems(element.getNodeName())) {
            Element e = XMLUtil.getUniqueChild(element, "web");
            if(e == null) e = XMLUtil.getUniqueChild(element, "WEB");
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
		String workspace = p.getProperty(XModelConstants.WORKSPACE);
		if(project == null) return;
		String relative = workspace.startsWith(project + "/") ? 
		    "." + workspace.substring(project.length()) : workspace;
		element.setAttribute("workspace-home", relative);
	}
	
	static Map<String,String> oldAttributes = new HashMap<String, String>();
	
	static {
		oldAttributes.put("application-name", "APPLICATION_NAME");
		oldAttributes.put("workspace-home", "WORKSPACE_HOME");
		oldAttributes.put("info", "INFO");
		oldAttributes.put("location", "LOCATION");
		oldAttributes.put("model-path", "MODEL_PATH");
		oldAttributes.put("root", "ROOT");
		oldAttributes.put("web-root", "root");
		oldAttributes.put("src", "SRC");
	}

    public String getAttribute(Element element, String xmlname, XAttribute attr) {
    	if(element == null || xmlname == null) return null;
    	if(!element.hasAttribute(xmlname)) {
    		String oldAttribute = xmlname;
    		while(oldAttribute != null) {
        		oldAttribute = oldAttributes.get(oldAttribute);
        		if(oldAttribute != null && element.hasAttribute(oldAttribute)) {
        			xmlname = oldAttribute;
        			break;
        		}
    		}
    	}
    	return super.getAttribute(element, xmlname, attr);
    }

}
