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
package org.jboss.tools.common.model.ui.views.navigator;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.jboss.tools.common.model.XFilteredTree;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.event.XModelTreeListener;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

/**
 * This cache is needed because content provider used in 
 * common navigator is stateless, and each call to its method 
 * needs to restore instance of XFilteredTree. It would take 
 * too much time to create it each time anew. 
 * @author glory
 */
public class FilteredTreesCache {
	static FilteredTreesCache instance = new FilteredTreesCache();
	
	public static FilteredTreesCache getInstance() {
		return instance;
	}
	
	Map<String, Map<XModel,XFilteredTree>> map = new HashMap<String, Map<XModel,XFilteredTree>>();
	
	private FilteredTreesCache() {}
	
	/**
	 * Takes existing tree instance from cache or creates one if needed. 
	 * @param name
	 * @param model
	 * @return
	 */
	public XFilteredTree getFilteredTree(String name, XModel model) {
		if(name == null || model == null) return null;
		Map<XModel,XFilteredTree> nmap = getNamedMap(name);
		XFilteredTree result = nmap.get(model);
		if(result == null) {
			result = createTree(name, model);
			if(result != null) nmap.put(model, result);
		}
		return result;
	}
	
	Map<XModel,XFilteredTree> getNamedMap(String name) {
		Map<XModel,XFilteredTree> nmap = map.get(name);
		if(nmap == null) {
			nmap = new HashMap<XModel, XFilteredTree>();
			map.put(name, nmap);
		}
		return nmap;
	}
	
	public void remove(XModel model) {
		if(model == null) return;
		Collection<Map<XModel,XFilteredTree>> i1 = map.values();
		for (Map<XModel,XFilteredTree> m: i1) {
			m.remove(model);
			for (XModelTreeListener l : listeners) {
				model.removeModelTreeListener(l);
			}			
		}
	}

	XFilteredTree createTree(String name, XModel model) {
		if(model == null || name == null) return null;
		String classname = model.getMetaData().getMapping("FilteredTrees").getValue(name);
		XFilteredTree result = null;
		try {
			result = (XFilteredTree)ModelFeatureFactory.getInstance().createFeatureInstance(classname);
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError(e);
			return null;
		}
		result.setModel(model);
		if(result.getRoot() == null) {
			result = null; 
			IProject p = EclipseResourceUtil.getProject(model.getRoot());
			IModelNature nature = EclipseResourceUtil.getModelNature(p);
			if(nature != null) {
				ModelUIPlugin.getPluginLog().logInfo("Red Hat Project " + p.getName() + " is corrupted.");
			}
		} 
		return result;
	}
	
	Set<XModelTreeListener> listeners = new HashSet<XModelTreeListener>();
	
	/**
	 * Registers listener with this cache.
	 * Using this cache by stateless content providers 
	 * makes it necessary to register listener with it,
	 * rather than just add it to model. Then, by 
	 * unregistering the listener while disposing content 
	 * provider, we can be sure that the listener is removed 
	 * from all models to which it has been added.
	 * @param listener
	 * @param model
	 */	
	public void addListener(XModelTreeListener listener, XModel model) {
		listeners.add(listener);
		model.addModelTreeListener(listener);
	}
	
	/**
	 * Unregisters listener from this cache.
	 * The listener is removed from all models 
	 * to which it has been added.
	 **/
	public void removeListener(XModelTreeListener listener) {
		listeners.remove(listener);
		Collection<Map<XModel,XFilteredTree>> i1 = map.values();
		for (Map<XModel,XFilteredTree> m: i1) {
			Collection<XFilteredTree> i2 = m.values();
			for (XFilteredTree t : i2) {
				XModel model = t.getRoot().getModel();
				if(model != null) model.removeModelTreeListener(listener);
			}
		}
	}	

}
