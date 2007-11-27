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
package org.jboss.tools.common.model.test;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import junit.framework.TestCase;

import org.jboss.tools.common.meta.XChild;
import org.jboss.tools.common.meta.XMapping;
import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.meta.XModelMetaData;
import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.meta.action.XActionHandler;
import org.jboss.tools.common.meta.action.XActionItem;
import org.jboss.tools.common.meta.action.XActionList;
import org.jboss.tools.common.meta.impl.XExtensions;
import org.jboss.tools.common.meta.impl.XModelEntityImpl;
import org.jboss.tools.common.meta.impl.XModelMetaDataImpl;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.engines.impl.EnginesLoader;
import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.event.XModelTreeListener;
import org.jboss.tools.common.model.loaders.XObjectLoader;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.util.ClassLoaderUtil;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;

public class MetaModelTest extends TestCase {
	
	public MetaModelTest() {}
	
	protected void setUp() throws Exception {
	}
	
	/**
	 * Base test checking that meta model is loaded.
	 */	
	public void testMetaModelLoading() {
		XModelMetaData meta = XModelMetaDataImpl.getInstance();
		assertNotNull("Meta model is not loaded", meta);
	}
	
	/**
	 * Some common implementations of XModelObject has aliases listed in
	 * mapping named "Implementations". Check that these classes exist.
	 */
	public void testImplementationMapping() {
		XModelMetaData meta = XModelMetaDataImpl.getInstance();
		XMapping mapping = meta.getMapping("Implementations");
		String[] s = mapping.getKeys();
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < s.length; i++) {
			String cn = mapping.getValue(s[i]);
			Class c = ModelFeatureFactory.getInstance().getFeatureClass(cn);
			if(c == null) {
				sb.append(s[i] + ":" + cn).append(" cannot find class").append("\n");
			} else {
				try {
					XModelObject o = (XModelObject)c.newInstance();
				} catch (Exception e) {
					sb.append(s[i] + ":" + cn).append(" - cannot assign to XModelObject").append("\n");
				}
			}
		}
		assertTrue("Mapping 'Implementations' has wrong items.\n" +  sb.toString(), sb.length() == 0);
	}
	
	/**
	 * Some common action handlers of has aliases listed in
	 * mapping named "Implementations". Check that these classes exist
	 * and can be reduced to XActionHandler.
	 */
	public void testActionHandlersMapping() {
		XModelMetaData meta = XModelMetaDataImpl.getInstance();
		XMapping mapping = meta.getMapping("Handlers");
		String[] s = mapping.getKeys();
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < s.length; i++) {
			String cn = mapping.getValue(s[i]);
			Class c = ModelFeatureFactory.getInstance().getFeatureClass(cn);
			if(c == null) {
				sb.append(s[i] + ":" + cn).append(" cannot find class").append("\n");
			} else {
				try {
					XActionHandler o = (XActionHandler)c.newInstance();
				} catch (Exception e) {
					sb.append(s[i] + ":" + cn).append(" - cannot assign to XActionHandler").append("\n");
				}
			}
		}
		assertTrue("Mapping 'Implementations' has wrong items.\n" + sb.toString(), sb.length() == 0);
	}
	
	public void testEntityImplementations() {
		XModelMetaData meta = XModelMetaDataImpl.getInstance();
		String[] entities = meta.entities();
		StringBuilder sb = new StringBuilder();
		int errors = 0;
		for (int i = 0; i < entities.length; i++) {
			XModelEntity entity = meta.getEntity(entities[i]);
			//TODO - should be tested through public interface
			String error = entity.testImplementation();
			if(error != null) {
				errors++;
				sb.append(entity.getName()).append(" - ").append(error).append("\n");
			}
		}
		assertTrue("These " + errors + " entities have wrong implementations\n" + sb.toString(), errors == 0);
	}
	
	/**
	 * For each entity check that declared child entities reference existing entities.
	 */
	public void testEntityChildren() {
		XModelMetaData meta = XModelMetaDataImpl.getInstance();
		String[] entities = meta.entities();
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < entities.length; i++) {
			XModelEntity entity = meta.getEntity(entities[i]);
			XChild[] cs = entity.getChildren();
			for (int j = 0; j < cs.length; j++) {
				if(meta.getEntity(cs[j].getName()) == null) {
					sb.append(entities[i] + ":" + cs[j].getName()).append("\n");
				}
			}
		}
		assertTrue("These entity children reference unknown entities.\n" + sb.toString(), sb.length() == 0);
	}
	
	/**
	 * Test that declared in action handler exists and is instanceof AbstractHandler.
	 * For handlers that are instances of DefaultSpecialHandler, check that declared 
	 * support class exists and is instanceof SpecialWizardSupport.
	 */	
	public void testActionHandlers() {
		XModelMetaData meta = XModelMetaDataImpl.getInstance();
		String[] entities = meta.entities();
		Set<String> errors = new HashSet<String>();
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < entities.length; i++) {
			XModelEntity entity = meta.getEntity(entities[i]);
			collectActionsWithWrongHandlers(entity, entity.getActionList(), errors);
		}
		sb.append("These " + errors.size() + " actions have wrong handlers\n");
		Iterator<String> it = errors.iterator();
		while(it.hasNext()) {
			sb.append(it.next()).append("\n");
		}
		assertTrue(sb.toString(), errors.size() == 0);
	}
	
	void collectActionsWithWrongHandlers(XModelEntity entity, XActionItem item, Set<String> errors) {
		if(item instanceof XActionList) {
			XActionList list = (XActionList)item;
			XActionItem[] is = list.getActionItems();
			for (int i = 0; i < is.length; i++) collectActionsWithWrongHandlers(entity, is[i], errors);
		} else if(item instanceof XAction) {
			XAction action = (XAction)item;
			//TODO - should be tested through public interface
			String error = action.testHandler();
			if(error != null) {
				errors.add(entity.getName() + ":" + action.getPath() + " - " + error);
			}
			//TODO - should be tested through public interface
			error = action.testEntityData();
			if(error != null) {
				errors.add(entity.getName() + ":" + action.getPath() + " - " + error);
			}
		}
	}
	
	/**
	 * Check that extensions reference existing entities.
	 */
	public void testEntityExtensions() {
		XModelMetaDataImpl meta = (XModelMetaDataImpl)XModelMetaDataImpl.getInstance();
		XExtensions s = meta.getExtensions();
		//TODO - should be tested through public interface
		Set set = s.test(meta);
		String message = "These " + set.size() + " extensions reference unknown entities\n";
		Iterator it = set.iterator();
		while(it.hasNext()) {
			message += it.next() + "\n";
		}
		assertTrue(message, set.size() == 0);		
	}
	
	/**
	 * Check icons declared in entities.
	 */
	public void testEntityIcons() {
		XModelMetaData meta = XModelMetaDataImpl.getInstance();
		String[] entities = meta.entities();
		int entitiesWithoutIcons = 0;
		String firstEntityWithoutIcon = null;
		for (int i = 0; i < entities.length; i++) {
			XModelEntity entity = meta.getEntity(entities[i]);
			String iconPath = entity.getRenderer().getIconInfo("main");
			if(iconPath != null) {
				Object o = meta.getIconList().getImage(iconPath);
				if(o == null) {
					entitiesWithoutIcons++;
					if(firstEntityWithoutIcon == null) firstEntityWithoutIcon = entities[i];
				}
			}
		}
		assertTrue("Icons for " + entitiesWithoutIcons + " entities are missing. Entity example: " + firstEntityWithoutIcon, entitiesWithoutIcons == 0);
	}
	
	/**
	 * Check icons declared in actions and cascade action lists.
	 */
	public void testActionIcons() {
		XModelMetaData meta = XModelMetaDataImpl.getInstance();
		String[] entities = meta.entities();
		Set<String> actionsWithoutIcons = new HashSet<String>();
		for (int i = 0; i < entities.length; i++) {
			XModelEntity entity = meta.getEntity(entities[i]);
			collectActionsWithoutIcon(entity, entity.getActionList(), actionsWithoutIcons);
			
		}
		StringBuffer message = new StringBuffer();
		message.append("Icons for " + actionsWithoutIcons.size() + " actions are missing.\n");
		Iterator<String> it = actionsWithoutIcons.iterator();
		while(it.hasNext()) {
			message.append("  ").append(it.next()).append("\n");
		}
		assertTrue(message.toString(), actionsWithoutIcons.size() == 0);
	}
	
	void collectActionsWithoutIcon(XModelEntity entity, XActionItem item, Set<String> actionsWithoutIcons) {
		String iconPath = item.getIconKey();
		if(iconPath != null && item.getPath() != null 
				&& iconPath.length() > 0 
				&& (!(item instanceof XActionList) || ((XActionList)item).getGroupFactor() == 1)) {
			Object o = item.getMetaModel().getIconList().getImage(iconPath);
			if(o == null) {
				actionsWithoutIcons.add("ActionPath=" + entity.getName() + ":" + item.getPath() + " iconPath=" + iconPath);
			}
		} else {
		}
		if(item instanceof XActionList) {
			XActionList list = (XActionList)item;
			XActionItem[] is = list.getActionItems();
			for (int i = 0; i < is.length; i++) collectActionsWithoutIcon(entity, is[i], actionsWithoutIcons);
		}
	}
	
	/**
	 * One everywhere available instance of model is preferences model.
	 * Check that it returns non-null instance.
	 */	
	public void testPreferencesModel() {
		XModel model = PreferenceModelUtilities.getPreferenceModel();
		assertTrue("Cannot load preferences model", model != null);
	}
	
	/**
	 * Try to create instance of model object for each entity that declares implementing class.
	 */
	public void testCreatingModelObjects() {
		XModelMetaData meta = XModelMetaDataImpl.getInstance();
		XModel model = PreferenceModelUtilities.getPreferenceModel();
		String[] entities = meta.entities();
		StringBuilder sb = new StringBuilder();
		int errors = 0;
		for (int i = 0; i < entities.length; i++) {
			XModelEntityImpl entity = (XModelEntityImpl)meta.getEntity(entities[i]);
			Class cls = entity.getImplementingClass();
			if(cls != null) {
				XModelObject object = model.createModelObject(entity.getName(), null);
				if(object == null) {
					errors++;
					String error = entity.getName() + ":" + cls.getName();
					sb.append(entity.getName()).append(" - ").append(error).append("\n");
				}
			}
		}
		assertTrue("Model objects for " + errors + " entities cannot be created\n" + sb.toString(), errors == 0);
		
	}
	
	/**
	 * Test loaders for all entities. If entity declares loader that cannot be created 
	 * and reduced to XObjectLoader, report error.
	 * For entities that represent file objects, create loader and model object instances,
	 * and do save/load, than compare initial object to object restored on loading. 
	 * If these objects are not identical, report error.
	 * TASK: It is good to have initial object as loaded from a large example file, 
	 * rather than just created 'empty' object. To provide for that, we can for example 
	 * introduce attribute 'example' to entity.
	 */	
	public void testObjectLoaders() {
		XModel model = PreferenceModelUtilities.getPreferenceModel();
		XModelMetaData meta = XModelMetaDataImpl.getInstance();
		String[] entities = meta.entities();
		StringBuilder sb = new StringBuilder();
		int errors = 0;
		for (int i = 0; i < entities.length; i++) {
			XModelEntity entity = meta.getEntity(entities[i]);
			//TODO - should be tested through public interface
			String error = entity.testLoader();
			if(error != null) {
				errors++;
				sb.append(entity.getName()).append(" - ").append(error).append("\n");
			} else {
				Class cls = entity.getLoadingClass();
				if(cls == null) continue;
				XModelObject object = XModelObjectLoaderUtil.createValidObject(model, entity.getName());
				if(object == null) {
					continue;
				}
				XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(object);
				if(object.getFileType() == XModelObject.FILE) {
					object.setModified(true);
					loader.save(object);
					String body = XModelObjectLoaderUtil.getTempBody(object);
					if(body == null) {
						errors++;
						sb.append(entity.getName()).append(" - ").append(" cannot save").append("\n");
					} else {
						XModelObject object2 = XModelObjectLoaderUtil.createValidObject(model, entity.getName());
						XModelObjectLoaderUtil.setTempBody(object2, body);
						loader.load(object2);
						MergeListener m = new MergeListener(model);
						EnginesLoader.merge(object, object2);
						m.dispose();
						if(!m.equal) {
							errors++;
							sb.append(entity.getName()).append(" - ").append(" save/load corrupts object").append("\n");
						}						
					}

				}
			}
		}
		assertTrue("These " + errors + " entities have wrong loaders\n" + sb.toString(), errors == 0);
	}
	
	class MergeListener implements XModelTreeListener {
		XModel model;
		boolean equal = true;
		
		public MergeListener(XModel model) {
			this.model = model;
			model.addModelTreeListener(this);
		}

		public void nodeChanged(XModelTreeEvent event) {
			equal = false;			
		}

		public void structureChanged(XModelTreeEvent event) {
			equal = false;			
		}
		
		public void dispose() {
			model.removeModelTreeListener(this);
		}
		
	}
		
}
