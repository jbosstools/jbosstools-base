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
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.constraint.XAttributeConstraintT;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public abstract class AllResourcesTree implements XFilteredTree {
	protected XModel preferenceModel;
	protected XModelObject root;
	protected Map<String,XFilteredTree> trees = new TreeMap<String,XFilteredTree>();
	protected XModelObject[] roots = null;
	
	public void setModel(XModel model) {
		preferenceModel = PreferenceModelUtilities.getPreferenceModel();
		root = preferenceModel.getRoot();
		roots = null;
	}

	public XModelObject getRoot() {
		return root;
	}

	public boolean hasChildren(XModelObject object) {
		if(object == root) return true;
		XFilteredTree tree = getSubTree(object.getModel());
		if(tree != null) for (int i = 0; i < roots.length; i++) {
			if(object == roots[i]) {
				object = tree.getRoot(); 
			}
		}
		return (tree != null) && tree.hasChildren(object);
	}
	
	private void loadRoots() {
		if(roots != null) return;
		roots = new XModelObject[trees.size()];
		Iterator it = trees.keySet().iterator();
		for (int i = 0; i < roots.length; i++) {
			XFilteredTree tree = trees.get(it.next());
			roots[i] = tree.getRoot().getParent();
		}
	}

	public XModelObject[] getChildren(XModelObject object) {
		loadRoots();
		if(object == root) {
			return roots;
		}
		XFilteredTree tree = getSubTree(object.getModel());
		if(tree != null) for (int i = 0; i < roots.length; i++) {
			if(object == roots[i]) {
				object = tree.getRoot(); 
			}
		}
		return tree == null ? new XModelObject[0] : tree.getChildren(object);
	}

	public XModelObject getChildAt(XModelObject object, int i) {
		loadRoots();
		if(object == root) {
			return roots[i];
		}
		XFilteredTree tree = getSubTree(object.getModel());
		if(tree != null) for (int k = 0; k < roots.length; k++) {
			if(object == roots[k]) {
				object = tree.getRoot(); 
			}
		}
		return tree == null ? null : tree.getChildAt(object, i);
	}

	public boolean isSelectable(XModelObject object) {
		if(object == root) return false;
		XFilteredTree tree = getSubTree(object.getModel());
		return (tree != null) && tree.isSelectable(object);
	}

	public String getValue(XModelObject object) {
		if(object == root) return "";
		IResource resource = (IResource)object.getAdapter(IResource.class);
		if(resource == null) {
			XFilteredTree tree = getSubTree(object.getModel());
			return "/" + ((IProject)object.getModel().getProperties().get("project")).getName() + "/" +  tree.getValue(object);
		}
		return (resource == null) ? "" : resource.getFullPath().toString();
	}

	public void setConstraint(Object object) {
        Object[] os = (Object[])object;
        XAttribute a = (XAttribute)os[0];
        XAttributeConstraintT tc = (XAttributeConstraintT)a.getConstraint();
        String nature = tc.getProperties().getProperty("nature");
        IProject[] ps = ModelPlugin.getWorkspace().getRoot().getProjects();
        for (int i = 0; i < ps.length; i++) {
        	if(!ps[i].isOpen()) continue;
        	try {
        		if(nature != null && !ps[i].hasNature(nature)) continue;
        	} catch (Exception e) {
        		if(ModelPlugin.isDebugEnabled()){
        			ModelPlugin.getPluginLog().logInfo("AllResourcesTree:" + e.getMessage());
        		}
        		continue;
        	}
    		IModelNature sp = EclipseResourceUtil.getModelNature(ps[i]);
    		if(sp == null) continue;
    		XModel model_i = sp.getModel();
    		XFilteredTree tree = createProjectTree();
    		tree.setModel(model_i);
    		tree.setConstraint(object);
    		if(tree.getRoot() == null) {
    			continue;
    		}
    		trees.put(ps[i].getName(), tree);
        }
	}
	
	protected abstract XFilteredTree createProjectTree();
	
	private XFilteredTree getSubTree(XModel model) {
		return trees.get(((IProject)model.getProperties().get("project")).getName());
	}

	public XModelObject find(String value) {
		if(value == null || value.length() == 0) return root;
		if(value.indexOf("//") >= 0) {
			String pn = value.substring(1, value.indexOf("//"));
			XFilteredTree tree = trees.get(pn);
			return (tree == null) ? null : tree.getRoot().getModel().getByPath(value.substring(value.indexOf("//") + 1));
		}
		if(value.indexOf("/", 1) < 0) return null;
		IResource r = null;
		try {
			r = ModelPlugin.getWorkspace().getRoot().getFolder(new Path(value));
		} catch (Exception e) {
			if(ModelPlugin.isDebugEnabled()){
				ModelPlugin.getPluginLog().logInfo("AllResourcesTree:" + e.getMessage());
    		}
		}
		if(r == null || !r.exists()) {
			try {
				r = ModelPlugin.getWorkspace().getRoot().getFile(new Path(value));
			} catch (Exception e) {
				if(ModelPlugin.isDebugEnabled()){
					ModelPlugin.getPluginLog().logInfo("AllResourcesTree:" + e.getMessage());
        		}
			}
		}
		if(r == null || !r.exists()) return null;
		return EclipseResourceUtil.getObjectByResource(r);
	}

	public XModelObject getParent(XModelObject object) {
		if(object == root) return null;
		XFilteredTree tree = getSubTree(object.getModel());
		if(tree == null) return null;
		if(tree.getRoot() == object) return root;
		XModelObject p = tree.getParent(object);
		if(p == tree.getRoot()) return p.getParent();
		return p;
	}

	public String getPath(XModelObject object) {
		if(object == root) return "";
		IResource resource = (IResource)object.getAdapter(IResource.class);
		return (resource == null) ? "" : resource.getFullPath().toString();
	}

	public void dispose() {
	}

}
