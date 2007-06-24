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
package org.jboss.tools.common.model.impl.trees;

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.constraint.*;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;

public class FileSystemResourceTree implements XFilteredTree {
    protected XModel model = null;
    protected Set<String> extensions = null;
    protected Set<String> entities = null;

    public FileSystemResourceTree() {}
    
    public void addExtension(String name) {
    	if(extensions == null) {
    		extensions = new HashSet<String>();
    	}
    	extensions.add(name);
    }

    public void dispose() {
    	if (extensions!=null) extensions.clear();
    	extensions = null;
    	if (entities!=null) entities.clear();
    	entities = null;
    }

    public void setModel(XModel model) {
        this.model = model;
    }

    public void setConstraint(Object object) {
        Object[] os = (Object[])object;
        XAttribute a = (XAttribute)os[0];
        XAttributeConstraintT tc = (XAttributeConstraintT)a.getConstraint();
        String ext = tc.getProperties().getProperty("extensions");
        if(ext != null) {
            extensions = new HashSet<String>();
            StringTokenizer st = new StringTokenizer(ext, ",");
            while(st.hasMoreTokens()) extensions.add(st.nextToken());
        }
        String ent = tc.getProperties().getProperty("entities");
        if(ent != null) {
            entities = new HashSet<String>();
            StringTokenizer st = new StringTokenizer(ent, ",");
            while(st.hasMoreTokens()) entities.add(st.nextToken());
        }
    }

    public XModelObject getRoot() {
        return FileSystemsHelper.getFileSystems(model);
    }

    public boolean isSelectable(XModelObject object) {
        return (object != null && object.getFileType() == XFileObject.FILE);
    }

    public String getValue(XModelObject object) {
        return getPath(object);
    }

    public XModelObject find(String value) {
        return model.getByPath(value);
    }

    public XModelObject getParent(XModelObject object) {
        XModelObject root = getRoot();
        if(object == root) return null;
        XModelObject p = object.getParent();
        return (p == null) ? null : (p.getParent() == root) ? root : p;
    }

    public String getPath(XModelObject object) {
        String p = object.getPath();
        if(p == null || !p.startsWith("FileSystems/")) return p;
        return XModelObjectLoaderUtil.getResourcePath(object);
    }

    public boolean hasChildren(XModelObject object) {
        if(object.getFileType() == XFileObject.FILE) return false;
        return true;
    }

    public XModelObject getChildAt(XModelObject object, int i) {
        XModelObject[] c = getChildren(object);
        return (i < 0 || i >= c.length) ? null : c[i];
    }

    public XModelObject[] getChildren(XModelObject object) {
        if(object == getRoot()) return getRootChildren();
        int t = object.getFileType();
        if(t == XFileObject.FILE) return new XModelObject[0];
        return getChildrenInFileSystems(object);
    }

    protected boolean accepts0(XModelObject o) {
        int type = o.getFileType();
        if(type == XModelObject.FOLDER) return !"true".equals(o.get("overlapped"));
        if(type != XModelObject.FILE) return false;
        String pathpart = o.getPathPart();
        String pp = pathpart.substring(pathpart.lastIndexOf('.') + 1);
        String ent = o.getModelEntity().getName();
        return (extensions == null || extensions.contains(pp)) &&
               (entities == null || entities.contains(ent));
    }

    private XModelObject[] getRootChildren() {
        XModelObject[] fs = getRoot().getChildren();
        SortedMap<String,XModelObject> t = new TreeMap<String,XModelObject>();
        for (int i = 0; i < fs.length; i++) {
            XModelObject[] cs = fs[i].getChildren();
            for (int j = 0; j < cs.length; j++) {
                String p = cs[j].getPathPart();
                if(t.containsKey(p)) continue;
                if(accepts0(cs[j])) t.put(p, cs[j]);
            }
        }
        Object[] keys = t.keySet().toArray();
        XModelObject[] vs = new XModelObject[keys.length];
        for (int i = 0; i < vs.length; i++) vs[i] = t.get(keys[i]);
        return vs;
    }

    private XModelObject[] getChildrenInFileSystems(XModelObject object) {
        String path = getPath(object);
        if(path.startsWith("/")) path = path.substring(1);
        XModelObject[] fs = getRoot().getChildren();
        SortedMap<String,XModelObject> t = new TreeMap<String,XModelObject>();
        for (int i = 0; i < fs.length; i++) {
            XModelObject o = fs[i].getChildByPath(path);
            if(o == null) continue;
            XModelObject[] cs = o.getChildren();
            for (int j = 0; j < cs.length; j++) {
                String p = cs[j].getPathPart();
                if(t.containsKey(p)) continue;
                if(accepts0(cs[j])) t.put(p, cs[j]);
            }
        }
        Object[] keys = t.keySet().toArray();
        XModelObject[] vs = new XModelObject[keys.length];
        for (int i = 0; i < vs.length; i++) vs[i] = t.get(keys[i]);
        return vs;
    }

}
