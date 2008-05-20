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

import org.jboss.tools.common.model.ui.forms.FormAttributeData;
import org.jboss.tools.common.model.ui.forms.FormData;
import org.jboss.tools.common.model.ui.forms.FormLayoutDataUtil;
import org.jboss.tools.common.model.ui.forms.IFormData;

public class MappingFormLayoutData implements MetaConstants {
	
	final static IFormData MAPPING_FOLDER_DEFINITION = new FormData(
		"Mappings",
		"", //"Description
		"Mappings",
		new FormAttributeData[]{new FormAttributeData("name", 100)},
		new String[]{MAPPING_ENTITY},
		FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateMapping")
	);

	final static IFormData MAPPING_LIST_DEFINITION = new FormData(
		"Mappings",
		"", //"Description
		MAPPINGS_ENTITY,
		new FormAttributeData[]{new FormAttributeData("name", 100)},
		new String[]{MAPPING_ENTITY},
		FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateMapping")
	);

	private final static IFormData[] MAPPING_DEFINITIONS = new IFormData[] {
		new FormData(
			"Mapping",
			"", //"Description
			FormLayoutDataUtil.createGeneralFormAttributeData(MAPPING_ENTITY)
		),
		new FormData(
			"Items",
			"", //"Description
			new FormAttributeData[]{new FormAttributeData("name", 30), new FormAttributeData("value", 70)},
			new String[]{MAPPING_ITEM_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateItem")
		)
	};

	final static IFormData MAPPING_DEFINITION = new FormData(
		MAPPING_ENTITY, new String[]{null}, MAPPING_DEFINITIONS
	);

}
