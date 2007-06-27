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
package org.jboss.tools.common.model.impl;

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.XMapping;
import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.meta.XModelMetaData;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.meta.action.impl.XEntityDataImpl;
import org.jboss.tools.common.meta.constraint.XAttributeConstraint;
import org.jboss.tools.common.meta.constraint.XAttributeConstraintV;
import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelBuffer;
import org.jboss.tools.common.model.XModelConstants;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelTransferBuffer;
import org.jboss.tools.common.model.event.XModelChangeManager;
import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.event.XModelTreeListener;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.filesystems.impl.FileSystemImpl;
import org.jboss.tools.common.model.filesystems.impl.FileSystemPeer;
import org.jboss.tools.common.model.loaders.EntityRecognizer;
import org.jboss.tools.common.model.loaders.XObjectLoader;
import org.jboss.tools.common.model.loaders.impl.ModelEntityRecognizer;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.undo.XChangeUndo;
import org.jboss.tools.common.model.undo.XUndoManager;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.model.util.XBundle;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;
import org.jboss.tools.common.model.util.XModelObjectUtil;
import org.jboss.tools.common.util.FileUtil;

public class XModelImpl implements XModel {
    private XModelMetaData metadata = null;
    private Properties properties = null;
    private ArrayList<XModelTreeListener> treeListeners = new ArrayList<XModelTreeListener>();
    private XModelTreeListener[] treeListenersArray = new XModelTreeListener[0];
    private XModelObject root = null;
    private XUndoManager undoer = new XUndoManager();
    private static ServiceDialog service = null;
    private static XModelBufferImpl buffer = new XModelBufferImpl();
    private static ModelEntityRecognizer recognizer = new ModelEntityRecognizer();
    private XModelChangeManager changemanager = new XModelChangeManager();
    private FileSystemPeer fileregistry = new FileSystemPeer();
    private PrintWriter out = new PrintWriter(System.out, true);
    private HashMap<String,XModelObject> extraroots = new HashMap<String,XModelObject>(2);
    private XModelClassLoader classloader = new XModelClassLoader(this);
    private String rootEntity = "Root";

    public XModelImpl(Properties properties, XModelMetaData metadata) {
        this.metadata = metadata;
        this.properties = properties;
        recognizer.setMetaData(metadata);
        if(properties.getProperty("rootEntity") != null) {
        	rootEntity = properties.getProperty("rootEntity");
        }
        XModelConstants.validate(this);
    }

    public XModelMetaData getMetaData() {
        return metadata;
    }

    public Properties getProperties() {
        return (Properties)properties;
    }
    
    public PrintWriter getOut() {
        return out;
    }

    public void setOut(PrintWriter out) {
        if(out != null) this.out = out;
    }

    public void setService(ServiceDialog _service) {
		service = _service;
        if(service != null) service.setModel(this);
    }

    public ServiceDialog getService() {
        return service;
    }

    public XUndoManager getUndoManager() {
        return undoer;
    }

    public XModelBuffer getModelBuffer() {
    	if(XModelTransferBuffer.getInstance().isEnabled()) {
    		return XModelTransferBuffer.getInstance().getBuffer();
    	}
        return buffer;
    }

    public EntityRecognizer getEntityRecognizer() {
        return recognizer;
    }

    public XModelChangeManager getChangeManager() {
        return changemanager;
    }

    public FileSystemPeer getFileRegistry() {
        return fileregistry;
    }

    public ClassLoader getModelClassLoader() {
        if(!classloader.isValid()) {
            classloader = new XModelClassLoader(this);
            classloader.validate();
        }
        return classloader;
    }

    public XModelObject getRoot() {
        if(root == null) createRoot();
        return root;
    }

    public XModelObject getRoot(String name) {
        if(name == null || name.length() == 0) return getRoot();
        XMapping m = getMetaData().getMapping("Roots");
        String path = m.getValue(name);
        return (path == null || path.length() == 0) ? null :
            getRoot().getChildByPath(path);
    }

    void createRoot() {
        root = createModelObject(rootEntity, new Properties());
    }

    public XModelObject getByPath(String path) {
        if(path == null || path.length() == 0) return getRoot();
        if(path.startsWith("root:")) {
            int i = path.indexOf('/');
            if(i < 0) return extraroots.get(path);
            XModelObject r = extraroots.get(path.substring(0, i));
            return (r == null) ? null : r.getChildByPath(path.substring(i + 1));
        }
        if(path.startsWith("/")) {
            return getByPathInFileSystem(path.substring(1));
        } else if(path.startsWith("%")) {
            int i = path.indexOf("%"), j = path.lastIndexOf("%");
            if(i == j) return null;
            String rt = path.substring(i + 1, j);
            XModelObject ro = getRoot(rt);
            return (ro == null || j + 2 >= path.length()) ? ro
                   : ro.getChildByPath(path.substring(j + 2));
        } else {
            return getRoot().getChildByPath(path);
        }
    }

    /*
     * If relative path goes into a folder overlapping a file system
     * the path is changed to relative with respect to that file system.
     */

    public static XModelObject getByRelativePath(XModel model, String path) {
        XModelObject o = null;
        if(path != null && path.startsWith("/")) {
        	XModelObject wr = FileSystemsHelper.getWebRoot(model);
        	if(wr != null) o = wr.getChildByPath(path.substring(1));
        }
        if(o == null) o = model.getByPath(path);
        if(o == null || !path.startsWith("/")) return o;
        XModelObject p = o;
        while(p != null && !"true".equals(p.get("overlapped"))) p = p.getParent();
        if(p == null) return o;
        path = o.getPath().substring(p.getPath().length());
        if(p.getModelEntity().getName().equals("FileFolder")) {
        	XModelObject fs = findMountedFileSystem(p);
        	if(fs != null && path.length() > 1) {
        		XModelObject c = fs.getChildByPath(path.substring(1));
        		if(c != null) return c;
        	}
        }
        return getByRelativePath(model, path);
    }
    
    static XModelObject findMountedFileSystem(XModelObject folder) {
    	String s = XModelObjectLoaderUtil.getResourcePath(folder);
    	if(s == null) return null;
    	XModelObject p = folder.getParent();
    	while(p != null && p.getFileType() != XModelObject.SYSTEM) p = p.getParent();
    	if(!(p instanceof FileSystemImpl)) return null;
    	String loc = ((FileSystemImpl)p).getAbsoluteLocation().replace('\\', '/');
    	XModelObject fs = FileSystemsHelper.getFileSystems(folder.getModel());
        XModelObject[] cs = fs.getChildren("FileSystemFolder");
        for (int i = 0; i < cs.length; i++) {
        	if(!(cs[i] instanceof FileSystemImpl)) continue;
        	String loci = ((FileSystemImpl)cs[i]).getAbsoluteLocation().replace('\\', '/');
        	if((loc + s).equals(loci)) return cs[i];
        }    	
    	return null;
    }

    public XModelObject getByPathInFileSystem(String path) {
    	XModelObject fs = getByPath("FileSystems");
    	if(fs == null) return null;
        XModelObject[] cs = fs.getChildren();
        for (int i = 0; i < cs.length; i++) {
            XModelObject o = cs[i].getChildByPath(path);
            if(o != null) return o;
        }
        return null;
    }
    
    static Set<String> unknownEntities = new HashSet<String>(); 

    public XModelObject createModelObject(String entity, Properties properties) {
        XModelEntity ent = getMetaData().getEntity(entity);
        if(ent == null) {
        	if(!unknownEntities.contains(entity)) {
        		unknownEntities.add(entity);
        		String message = XBundle.getInstance().getMessage("model", "UNKNOUN_ENTITY", new Object[]{entity});
        		ModelPlugin.getPluginLog().logInfo(message);
        	}
            return null;
        }
        try {
            Class c = ent.getImplementingClass();
            XModelObjectImpl me = (XModelObjectImpl)c.newInstance();
            me.setModel(this);
            me.setEntityName_0(entity);
            XAttribute[] an = ent.getAttributes();
            for (int i = 0; i < an.length; i++) {
                String n = an[i].getName();
                String v = an[i].getDefaultValue();
                me.set_0(n, v);
                if(properties != null) {
                    v = properties.getProperty(n);
                    if(v != null) me.set_0(n, v);
                }
            }
            return me;
        } catch (Exception e) {
        	if(!unknownEntities.contains(entity)) {
        		unknownEntities.add(entity);
        		String message = XBundle.getInstance().getMessage("model", "CREATION_ENTITY_FAILURE", new Object[]{entity});
        		ModelPlugin.getPluginLog().logError(message, e);
        	}
        }
        return null;
    }

	public void editObjectAttribute(XModelObject object, String attributeName, String value) {
		changeObjectAttribute(object, attributeName, value, true);
	}

	public void changeObjectAttribute(XModelObject object, String attributeName, String value) {
		changeObjectAttribute(object, attributeName, value, false);
	}
    
    void changeObjectAttribute(XModelObject object, String attributeName, String value, boolean edit) {
        if(object == null || object.getPath() == null) return;
        XModelEntity ent = object.getModelEntity();
        XAttribute a = ent.getAttribute(attributeName);
        if(a == null) return;
        if(a.isTrimmable() && value != null) value = value.trim();
        String ov = object.getAttributeValue(attributeName);
        ov = (ov == null) ? "" : ov;
        if(!isDifferent(ov, value)) return;
        if(value.length() == 0 && "true".equals(a.getProperty("required"))) {
        	String mes = "Attribute " + a.getName() + " is required.";
        	service.showDialog("Error",
                    mes, new String[]{"OK"}, null, ServiceDialog.ERROR);
        	return;
        }
        XAttributeConstraint c = a.getConstraint();
        if(c != null && service != null) {
            String error = (c instanceof XAttributeConstraintV)
                           ? ((XAttributeConstraintV)c).getError(value, object)
                           : c.getError(value);
            if(error != null) {
                if(edit) {
                    String mes = XBundle.getInstance().getMessage("model",
                                "SET_ATTRIBUTE_FAILURE",
                                new Object[]{attributeName, value, error});
                   if(service != null) {
                	   XEntityData data = XEntityDataImpl.create(new String[][]{
                			{object.getModelEntity().getName(), "yes"},
                			{attributeName, "no"}
                	   });
                	   data.setValue(attributeName, value);
                	   int q = service.showDialog("Error", mes, new String[]{"Finish", "Cancel"}, data, ServiceDialog.ERROR);
                	   if(q != 0) return;
                	   changeObjectAttribute(object, attributeName, data.getValue(attributeName), true);
                   }                                       
                   return;
                } else {
                    value = c.getCorrectedValue(value);
                    if(value == null) return;
                    if(!isDifferent(ov, value)) return;
                }
            }
        }
        String nv = object.setAttributeValue(attributeName, value);
        nv = (nv == null) ? "" : nv;
        ent.setDependentValues(object, attributeName);
        XChangeUndo cu = new XChangeUndo(object, attributeName, ov);
        undoer.addUndoable(cu);
        object.setModified(true);
        if(edit) {
        	((XModelObjectImpl)object).onAttributeValueEdit(attributeName, ov, nv);
        }
////        ent.getAttribute(attributeName).valueChanged(object);
    }
    
    private boolean isDifferent(String v1, String v2) {
    	if(v1 == null) return v2 != null;
    	if(v1.equals(v2)) return false;
    	int i1 = -1, i2 = -1, l1 = v1.length(), l2 = v2.length();
    	char c1 = '\0', c2 = '\0';
    	while(i1 < l1 || l2 < l2) {
    		if(i1 < l1) {
				++i1; while(i1 < l1 && (c1 = v1.charAt(i1)) == '\r') ++i1;
				if(i1 == l1) c1 = '\0';
			}
			if(i2 < l2) {
				++i2; while(i2 < l2 && (c2 = v2.charAt(i2)) == '\r') ++i2;
				if(i2 == l2) c2 = '\0';
			}
			if(c1 != c2) return true;
    	}
    	return false;
    }

    // listeners

    public synchronized void addModelTreeListener(XModelTreeListener listener) {
    	if(treeListeners.contains(listener)) return;
        treeListeners.add(listener);
		treeListenersArray = (XModelTreeListener[])treeListeners.toArray(new XModelTreeListener[0]);
    }

    public synchronized void removeModelTreeListener(XModelTreeListener listener) {
    	if(!treeListeners.contains(listener)) return;
        treeListeners.remove(listener);
		treeListenersArray = (XModelTreeListener[])treeListeners.toArray(new XModelTreeListener[0]);
    }

    // load - save - update

    public void load() {
        validateRootEntity();
        ArrayList<XModelTreeListener> tls = treeListeners;
		treeListenersArray = new XModelTreeListener[0];
        treeListeners = new ArrayList<XModelTreeListener>();
        XModelObject r = getRoot();
        XModelObjectLoaderUtil.getObjectLoader(r).load(r);
        treeListeners = tls;
		treeListenersArray = treeListeners.toArray(new XModelTreeListener[0]);
        undoer.setModel(this);
        undoer.reset();
        if(classloader.isUsed()) classloader = new XModelClassLoader(this);
        classloader.validate();
        fireStructureChanged(getRoot(), 3, null);
        if(!isDummy() && "true".equals(getProperties().getProperty("design"))) {
            OpenedProjectsImpl p = (OpenedProjectsImpl)getRoot().getChildByPath("Workspaces");
            p.last(XModelConstants.getWorkspace(this) + "/" + XModelConstants.getProjectPrefix(this) + "workspace.pex");
            if(p.isModified()) save();
        }
        ///Project Watcher
        if(!isDummy()) loadWatcher();
    }

        ////ProjectWatcher
        private void loadWatcher() {
            try {
            	if(getProperties().get("project") == null) return;
                XObjectLoader l = (XObjectLoader)ModelFeatureFactory.getInstance().createFeatureInstance("org.jboss.tools.common.model.project.WatcherLoader");
                XModelObject fs = getByPath("FileSystems");
                if(l != null && fs != null) l.load(fs);
            } catch (Exception t) {
            	ModelPlugin.getPluginLog().logError("XModelImpl:loadWatcher:" + t.getMessage());
            }
        }

    public boolean isDummy() {
        return "RootDummy".equals(getRoot().getModelEntity().getName());
    }

    private String reduceURLPath(String p) {
        return FileUtil.fileURLToFilePath(p);
    }

    private String getProjectName() {
        String d = properties.getProperty(XModelConstants.WORKSPACE);
        String n = properties.getProperty(XModelConstants.WORKSPACE_NAME);
        if(d == null) return null;
        d = reduceURLPath(d);
        if(d.lastIndexOf(':') >= 2)
          return (n != null) ? n : d.substring(d.lastIndexOf('/') + 1);
        File f = null;
        try {
            f = new File(d);
            f.mkdirs();
        } catch (Exception e) {
        	ModelPlugin.getPluginLog().logError("XModelImpl:getProjectName:" + e.getMessage());
        }
        return (f == null || !f.isDirectory()) ? null : (n != null) ? n : f.getName();
    }

    private void validateRootEntity() {
        XModelObjectImpl r = (XModelObjectImpl)getRoot();
        String project = getProjectName();
        if(project == null) {
            r.setEntityName_0("RootDummy");
        } else {
            r.setAttributeValue("project name", project);
            r.setEntityName_0(rootEntity);
        }
    }

    public void loadLastProject() {
        String workspace = properties.getProperty(XModelConstants.WORKSPACE);
        if(workspace != null && workspace.trim().length() > 0) return;
        load(getLastWorkspace());
    }

    private String[] getLastWorkspace() {
        XModelObject pr = getRoot().getChildByPath("Workspaces");
        if("no".equals(pr.getAttributeValue("open workspace"))) return null;
        XModelObject[] os = pr.getChildren();
        for (int i = 0; i < os.length; i++) {
            String s = XModelObjectUtil.getExpandedValue(os[i], "name", null);
            s = reduceURLPath(s);
            if(s.lastIndexOf(':') >= 2 || new File(s).exists()) continue;
            os[i].removeFromParent();
            pr.setModified(true);
        }
        if(pr.isModified()) save();
        os = pr.getChildren();
        if(os.length == 0) return null;
        String s = XModelObjectUtil.getExpandedValue(os[0], "name", null);
        s = reduceURLPath(s);
        int q = s.lastIndexOf('/');
        String ws = s.substring(0, q);
        String n = s.substring(q + 1);
        q = n.lastIndexOf('-');
        String wsn = (q < 0) ? null : n.substring(0, q);
        return new String[]{ws, wsn};
    }

    private void load(String[] workspace) {
        if(workspace == null || workspace[0] == null) return;
        XModelConstants.setWorkspace(this, workspace[0]);
        XModelConstants.setWorkspaceName(this, workspace[1]);
        load();
    }

    public void update() {
        XModelObject r = getRoot();
        XModelObjectLoaderUtil.getObjectLoader(r).update(r);
    }

    public void save() {
        XModelObject r = getRoot();
        r.set("isSaveOn", "true");
        XModelObjectLoaderUtil.getObjectLoader(r).save(r);
        r.set("isSaveOn", "");
    }

    public void saveOptions() {
        XModelObject o = getRoot("XStudio");
        if(o != null) XModelObjectLoaderUtil.getObjectLoader(o).save(o);
    }

    public void setExtraRoot(XModelObject r) {
        String pathpart = r.getPathPart();
        if(!pathpart.startsWith("root:")) throw new RuntimeException("Not extra root: " + pathpart);
        extraroots.put(pathpart, r);
    }

    public void removeExtraRoot(String pathpart) {
        extraroots.remove(pathpart);
    }

    // fire

    public void fireNodeChanged(XModelObject object, String info) {
		fireNodeChanged(object, info, null);
    }

	void fireNodeChanged(XModelObject object, String info, Object details) {
		if(object.getModel() != this || !object.isActive()) return;
		XModelTreeEvent event = new XModelTreeEvent(this, object, 0, info, details);
		XModelTreeListener[] ls = treeListenersArray;
		for (int i = 0; i < ls.length; i++) ls[i].nodeChanged(event);
	}

    public void fireStructureChanged(XModelObject object) {
        fireStructureChanged(object, XModelTreeEvent.STRUCTURE_CHANGED, null);
    }
    

    public void fireStructureChanged(XModelObject object, int kind, Object info) {
        if(object.getModel() != this || !object.isActive()) return;
        XModelTreeEvent event = new XModelTreeEvent(this, object, kind, info);
        XModelTreeListener[] ls = treeListenersArray;
        for (int i = 0; i < ls.length; i++) ls[i].structureChanged(event);
    }
    
    protected Map<String,Object> managers = new HashMap<String,Object>();
    
	public Object getManager(String id) {
		return managers.get(id);
	}
	
	public void addManager(String id, Object manager) {
		managers.put(id, manager);
	}
	
	public void removeManager(String id) {
		managers.remove(id);
	}

	int loaderCount = 0;
	
	Object loaderMonitor = new Object();
	
	public void addLoader() {
		synchronized(loaderMonitor) {
			loaderCount++;
		}
	}
	
	public void removeLoader() {
		synchronized(loaderMonitor) {
			loaderCount--;
			if(loaderCount <= 0) loaderMonitor.notifyAll();
		}
	}
	
	public void waitForLoading() {
		synchronized(loaderMonitor) {
			if(loaderCount <= 0) return;
			try {
				loaderMonitor.wait();
			} catch (Exception e) {
				//ignore
			}
		}
	}
	
	public List getTreeListeners() {
		return Collections.unmodifiableList(this.treeListeners);
	}
	
	public Map getManagerMap() {
		return Collections.unmodifiableMap(managers);
	}
}

