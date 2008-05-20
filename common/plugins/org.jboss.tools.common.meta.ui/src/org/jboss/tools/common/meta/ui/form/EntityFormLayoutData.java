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

public class EntityFormLayoutData implements MetaConstants {

	final static IFormData CHILDREN_FOLDER_DEFINITION = new FormData(
		"Children",
		"", //"Description
		"Children",
		new FormAttributeData[]{new FormAttributeData("name", 100)},
		new String[]{CHILD_ENTITY},
		FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateChild")
	);

	final static IFormData CHILDREN_LIST_DEFINITION = new FormData(
		"Children",
		"", //"Description
		CHILDREN_ENTITY,
		new FormAttributeData[]{new FormAttributeData("name", 100)},
		new String[]{CHILD_ENTITY},
		FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateChild")
	);

	private final static IFormData[] ENTITY_DEFINITIONS = new IFormData[] {
		new FormData(
			"Entity",
			"", //"Description
			FormLayoutDataUtil.createGeneralFormAttributeData(ENTITY_ENTITY)
		),
		CHILDREN_FOLDER_DEFINITION,
		AttributeFormLayoutData.ATTRIBUTES_FOLDER_DEFINITION,
		new FormData(
			"Action List",
			"", //"Description
			"ActionList",
			new FormAttributeData[]{new FormAttributeData("name", 70), new FormAttributeData("element type", 30)},
			new String[]{ACTION_LIST_ENTITY, ACTION_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.AddItem")
		),
		new FormData(
			"Advanced",
			"", //"Description
			FormLayoutDataUtil.createAdvancedFormAttributeData(ENTITY_ENTITY)
		)
	};
	
	final static IFormData ENTITY_DEFINITION = new FormData(
		ENTITY_ENTITY, new String[]{null}, ENTITY_DEFINITIONS
	);

}
