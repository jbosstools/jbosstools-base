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
package org.jboss.tools.common.model.ui.forms;

import java.util.*;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.meta.impl.XModelMetaDataImpl;
import org.jboss.tools.common.meta.key.WizardKeys;

public class ModelFormLayoutData implements IFormLayoutData {
	public static String EMPTY_DESCRIPTION = ""; //$NON-NLS-1$
	
	public static IFormData TAG_LIST = new FormData(
		"Tags",
		"", //"Description //$NON-NLS-1$
		new FormAttributeData[]{new FormAttributeData("tag", 100)}, //$NON-NLS-1$
		new String[]{"AnyElement"}, //$NON-NLS-1$
		FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateTag") //$NON-NLS-1$
	);

	private final static IFormData[] FORM_LAYOUT_DEFINITIONS =
		new IFormData[] {
			new FormData(
				"AnyElement", //$NON-NLS-1$
				new String[]{null},
				new IFormData[] {
					new FormData("org.jboss.tools.common.model.ui.forms.AnyElementForm"), //$NON-NLS-1$
					new FormData(
						"Body Content",
						"", //"Description, description, description", //$NON-NLS-1$
						new FormAttributeData[]{new FormAttributeData("text", InfoLayoutDataFactory.getInstance())} //$NON-NLS-1$
					),
					TAG_LIST
				}
			),
	};
	private static Map<String,IFormData> FORM_LAYOUT_DEFINITION_MAP = Collections.synchronizedMap(new ArrayToMap(FORM_LAYOUT_DEFINITIONS));

	private static final ModelFormLayoutData INSTANCE = new ModelFormLayoutData();

	public static ModelFormLayoutData getInstance() {
		return INSTANCE;
	}

	public IFormData getFormData(String entityName) {
		IFormData data = (IFormData)FORM_LAYOUT_DEFINITION_MAP.get(entityName);
		if(data == null) {
			data = generateDefaultFormData(entityName);
		}
		return data;
	}

	private IFormData generateDefaultFormData(String entityName) {
		IFormData data = null;
		XModelEntity entity = XModelMetaDataImpl.getInstance().getEntity(entityName);
		if(entity != null) {
			data = generateDefaultFormData(entity);
		}
		if(data != null) {
			FORM_LAYOUT_DEFINITION_MAP.put(entityName, data);
		}
		return data;		
	}
	
	/**
	 * Returns form data that has field editors for attributes 
	 * with property category=general
	 * @param entity
	 * @return
	 */	
	public static IFormData createGeneralFormData(XModelEntity entity) {
		String entityName = entity.getName();
		XAttribute attr = entity.getAttribute("element type"); //$NON-NLS-1$
		String kind = attr == null ? entity.getXMLSubPath() : attr.getDefaultValue();
		String label = WizardKeys.toDisplayName(kind);
		IFormAttributeData[] attrData = FormLayoutDataUtil.createGeneralFormAttributeData(entityName);
		if(attrData.length == 0) return null;
		FormData g = new FormData(label, EMPTY_DESCRIPTION, attrData);
		return g;
	}

	/**
	 * Returns form data that has field editors for attributes 
	 * with property category=advanced
	 * @param entity
	 * @return
	 */	
	public static IFormData createAdvancedFormData(String entityName) {
		IFormAttributeData[] attrData = FormLayoutDataUtil.createAdvancedFormAttributeData(entityName);
		if(attrData.length == 0) return null;
		FormData a = new FormData(
			"Advanced",
			EMPTY_DESCRIPTION,
			FormLayoutDataUtil.createAdvancedFormAttributeData(entityName)
		);
		return a;
	}

	private IFormData generateDefaultFormData(XModelEntity entity) {
		String entityName = entity.getName();
		List<IFormData> list = new ArrayList<IFormData>();
		IFormData g = createGeneralFormData(entity);
		if(g != null) list.add(g);
		IFormData a = createAdvancedFormData(entityName);
		if(a != null) list.add(a);
		IFormData[] ds = list.toArray(new IFormData[0]);
		IFormData data = new FormData(entityName, new String[]{null}, ds);
		return data;
	}

}
