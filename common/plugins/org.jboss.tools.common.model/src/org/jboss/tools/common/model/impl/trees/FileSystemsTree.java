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
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.filesystems.impl.*;

public class FileSystemsTree extends DefaultSiftedTree {
    private XFilteredTreeConstraint[] constraints = new XFilteredTreeConstraint[0];
    protected String prefix = createPrefix();

    public FileSystemsTree() {}

	public void dispose() {}
    
    protected String createPrefix() {
        return "FileSystems$";
    }

    public void setModel(XModel model) {
        this.model = model;
        XModelObject root = getRoot();
        if(root instanceof FileSystemsImpl) {
            FileSystemsImpl fs = (FileSystemsImpl)root;
            fs.updateOverlapped();
        }
        load();
    }

    public XModelObject getRoot() {
        return model.getByPath("FileSystems");
    }

    private void load() {
        XMapping m = model.getMetaData().getMapping("FilteredTreeConstraints");
        if(m == null) return;
        ArrayList<XFilteredTreeConstraint> l = new ArrayList<XFilteredTreeConstraint>();
        String[] ks = m.getKeys();
        for (int i = 0; i < ks.length; i++) {
            if(!isConstraintRelevant(ks[i])) continue;
            String v = m.getValue(ks[i]);
            XFilteredTreeConstraint c = null;
            try {
                c = (XFilteredTreeConstraint)ModelFeatureFactory.getInstance().createFeatureInstance(v);
            } catch (Exception e) {
            	ModelPlugin.getPluginLog().logError(e);
            }
            if(c == null) continue;
            c.update(model);
            l.add(c);
        }
        constraints = (XFilteredTreeConstraint[])l.toArray(new XFilteredTreeConstraint[l.size()]);
    }
    
    protected boolean isConstraintRelevant(String key) {
    	return key.startsWith(prefix);
    }

    public boolean hasChildren(XModelObject object) {
///    	if(object.getFileType() == XModelObject.FILE && "yes".equals(object.get("_hasErrors_"))) return false;
        for (int i = 0; i < constraints.length; i++)
          if(constraints[i].isHidingAllChildren(object)) return false;
        return super.hasChildren(object);
    }

    public XModelObject[] getChildren(XModelObject object) {
        if(!hasChildren(object)) return new XModelObject[0];
        for (int i = 0; i < constraints.length; i++)
          if(constraints[i].isHidingSomeChildren(object)) return getNonHiddenChildren(object);
        return super.getChildren(object);
    }

    private XModelObject[] getNonHiddenChildren(XModelObject object) {
        ArrayList<XModelObject> l = new ArrayList<XModelObject>();
        XModelObject[] os = object.getChildren();
        for (int i = 0; i < os.length; i++) if(accepts(os[i])) l.add(os[i]);
        return l.toArray(new XModelObject[l.size()]);
    }

    private boolean accepts(XModelObject object) {
        for (int i = 0; i < constraints.length; i++)
          if(!constraints[i].accepts(object)) return false;
        return true;
    }
    
    /**
     * Tree may represent some model object with another model object
     * to build new hierarchy, e.g. bean object representing both 
     * java file and managed bean in jsf tree.
     * @param object
     * @return
     */    
    public XModelObject getRepresentation(XModelObject object) {
    	return object;
    }

}

