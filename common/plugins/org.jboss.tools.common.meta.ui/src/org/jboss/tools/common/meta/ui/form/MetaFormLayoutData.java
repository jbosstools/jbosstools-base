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
package org.jboss.tools.common.meta.ui.form;

import java.util.*;
import org.jboss.tools.common.model.util.ClassLoaderUtil;
import org.jboss.tools.common.model.ui.forms.*;

public class MetaFormLayoutData implements IFormLayoutData {
	static {
		ClassLoaderUtil.init();
	}

	private final static IFormData[] FORM_LAYOUT_DEFINITIONS = new IFormData[] {
		GroupFormLayoutData.GROUP_DEFINITION,
		EntityFormLayoutData.ENTITY_DEFINITION,
		ExtensionFormLayoutData.EXTENSION_DEFINITION,
		EntityFormLayoutData.CHILDREN_LIST_DEFINITION,
	
		ActionListFormLayoutData.ACTION_LIST_DEFINITION,
		ActionListFormLayoutData.ACTION_DEFINITION,
		ActionListFormLayoutData.ENTITY_DATA_DEFINITION,
	
		AttributeFormLayoutData.ATTRIBUTES_LIST_DEFINITION,
		AttributeFormLayoutData.ATTRIBUTE_DEFINITION,
		AttributeFormLayoutData.ATTRIBUTE_REF_DEFINITION,
		AttributeFormLayoutData.ATTRIBUTE_CONSTRAINT_DEFINITION,
		
		MappingFormLayoutData.MAPPING_LIST_DEFINITION,
		MappingFormLayoutData.MAPPING_DEFINITION,
		
		IconFormLayoutData.ICON_GROUP_DEFINITION,
		IconFormLayoutData.ICONS_DEFINITION,
	};

	private static Map FORM_LAYOUT_DEFINITION_MAP = Collections.unmodifiableMap(new ArrayToMap(FORM_LAYOUT_DEFINITIONS));
	
	static MetaFormLayoutData INSTANCE = new MetaFormLayoutData();
	
	public static IFormLayoutData getInstance() {
		return INSTANCE;
	}
	
	public MetaFormLayoutData() {}

	public IFormData getFormData(String entityName) {
		return (IFormData)FORM_LAYOUT_DEFINITION_MAP.get(entityName);
	}

}
