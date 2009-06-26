/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.navigator.decorator;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.preference.IPreferenceStore;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

/**
 * @author Viacheslav Kabanovich
 */
public class DecoratorManager {
	public static final String EXTENSION_POINT_ID = ModelUIPlugin.PLUGIN_ID + ".labelDecorator"; //$NON-NLS-1$
	
	private Map<String, XModelObjectDecorator> mapByName = new HashMap<String, XModelObjectDecorator>();
	private Map<String, XModelObjectDecorator> mapByEntity = new HashMap<String, XModelObjectDecorator>();
	private Map<String, Set<XModelObjectDecorator>> mapByPartition = new HashMap<String, Set<XModelObjectDecorator>>();
	private Map<String, String> entityByPartition = new HashMap<String, String>();
	
	private DecoratorManager() {
		load();
	}
	
	public static DecoratorManager getInstance() {
		return DecoratorManagerHolder.INSTANCE;
	}
	
	public static class DecoratorManagerHolder {
		public static DecoratorManager INSTANCE = new DecoratorManager();
	}
	
	public XModelObjectDecorator getDecoratorByEntity(String entity) {
		return mapByEntity.get(entity);
	}
	
	public Set<XModelObjectDecorator> getDecoratorsByPartition(String partition) {
		return mapByPartition.get(partition);
	}
	
	public String getBaseEntityForPartition(String partition) {
		return entityByPartition.get(partition);
	}

	public String[] getPartitions() {
		Set<String> set = new TreeSet<String>();
		set.addAll(mapByPartition.keySet());
		return set.toArray(new String[0]);
	}
	
	private void load() {
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(EXTENSION_POINT_ID);
		if(point == null) return;
		IExtension[] es = point.getExtensions();
		if(es != null) for (int i = 0; i < es.length; i++) {
			IConfigurationElement[] elements = es[i].getConfigurationElements();
			for (int j = 0; j < elements.length; j++) {
				XModelObjectDecorator d = new XModelObjectDecorator();
				d.load(elements[j]);
				String name = d.getName();
				String partition = d.getPartition();
				Set<String> entities = d.getEntities();
				mapByName.put(name, d);
				Set<XModelObjectDecorator> set = mapByPartition.get(partition);
				if(set == null) {
					set = new HashSet<XModelObjectDecorator>();
					mapByPartition.put(partition, set);
				}
				set.add(d);
				for (String entity: entities) {
					mapByEntity.put(entity, d);
					if(entity.startsWith("File") && entityByPartition.get(partition) == null) { //$NON-NLS-1$
						entityByPartition.put(partition, entity);
					}
				}
			}
		}
		loadFromPreferences();
	}
	
	public void loadFromPreferences() {
		IPreferenceStore p = getPreferences();
		for (String name: mapByName.keySet()) {
			XModelObjectDecorator d = mapByName.get(name);
			String key = ModelUIPlugin.PLUGIN_ID + ".labelDecorator." + name; //$NON-NLS-1$
			String value = p.getString(key);
			if(value != null && value.length() > 0) {
				d.setValue(value);
			}
		}
	}
	
	public void applyToPreferences() {
		IPreferenceStore p = getPreferences();
		for (String name: mapByName.keySet()) {
			XModelObjectDecorator d = mapByName.get(name);
			String key = ModelUIPlugin.PLUGIN_ID + ".labelDecorator." + name; //$NON-NLS-1$
			String value = d.getValue();
			if(value != null && !value.equals(d.getDefaultValue()) && !value.equals("{name}")) { //$NON-NLS-1$
				p.setValue(key, value);			
			} else {
				p.setValue(key, ""); //$NON-NLS-1$
			}
		}
		if(p instanceof IPersistentPreferenceStore) {
			try {
				((IPersistentPreferenceStore) p).save();
			} catch (IOException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
		}
	}

	public void setDefaults() {
		for (String name: mapByName.keySet()) {
			XModelObjectDecorator d = mapByName.get(name);
			d.setValue(d.getDefaultValue());
		}
	}
	
	public static IPreferenceStore getPreferences() {
		return ModelUIPlugin.getDefault().getPreferenceStore();
	}

}
