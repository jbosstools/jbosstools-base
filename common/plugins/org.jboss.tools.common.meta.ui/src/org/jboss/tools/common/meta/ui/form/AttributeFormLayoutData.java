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

public class AttributeFormLayoutData implements MetaConstants {

	private final static IFormData[] ATTRIBUTE_DEFINITIONS = new IFormData[] {
		new FormData(
			"Attribute",
			"", //"Description
			FormLayoutDataUtil.createGeneralFormAttributeData(ATTRIBUTE_ENTITY)
		),
		new FormData(
			"Constraint",
			"", //"Description
			"Constraint",
			FormLayoutDataUtil.createGeneralFormAttributeData(ATTRIBUTE_CONSTRAINT_ENTITY)
		),
		new FormData(
			"Editor",
			"", //"Description
			"Editor",
			FormLayoutDataUtil.createGeneralFormAttributeData(ATTRIBUTE_EDITOR_ENTITY)
		),
		new FormData(
			"Advanced",
			"", //"Description
			FormLayoutDataUtil.createAdvancedFormAttributeData(ATTRIBUTE_ENTITY)
		),
	};

	private final static IFormData[] ATTRIBUTE_REF_DEFINITIONS = new IFormData[] {
		new FormData(
			"Attribute Reference",
			"", //"Description
			FormLayoutDataUtil.createGeneralFormAttributeData(ATTRIBUTE_REF_ENTITY)
		),
	};

	final static IFormData ATTRIBUTES_FOLDER_DEFINITION = new FormData(
		"Attributes",
		"", //"Description,
		"Attributes",
		new FormAttributeData[]{new FormAttributeData("name", 100)},
		new String[]{ATTRIBUTE_ENTITY,ATTRIBUTE_REF_ENTITY},
		FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateAttribute")
	);

	final static IFormData ATTRIBUTES_LIST_DEFINITION = new FormData(
		"Attributes",
		"", //"Description,
		ATTRIBUTES_ENTITY,
		new FormAttributeData[]{new FormAttributeData("name", 100)},
		new String[]{ATTRIBUTE_ENTITY,ATTRIBUTE_REF_ENTITY},
		FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateAttribute")
	);

	final static IFormData ATTRIBUTE_DEFINITION = new FormData(
		ATTRIBUTE_ENTITY, new String[]{null}, ATTRIBUTE_DEFINITIONS
	);

	final static IFormData ATTRIBUTE_REF_DEFINITION = new FormData(
		ATTRIBUTE_REF_ENTITY, new String[]{null}, ATTRIBUTE_REF_DEFINITIONS
	);

	private final static IFormData[] ATTRIBUTE_CONSTRAINT_DEFINITIONS = new IFormData[] {
		new FormData(
			"Constraint",
			"", //"Description
			FormLayoutDataUtil.createGeneralFormAttributeData(ATTRIBUTE_CONSTRAINT_ENTITY)
		),
		new FormData(
			"Items",
			"", //"Description
			new FormAttributeData[]{new FormAttributeData("name", 100)},
			new String[]{CONSTRAINT_ITEM_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateConstraintItem")
		),
	};

	final static IFormData ATTRIBUTE_CONSTRAINT_DEFINITION = new FormData(
		ATTRIBUTE_CONSTRAINT_ENTITY, new String[]{null}, ATTRIBUTE_CONSTRAINT_DEFINITIONS
	);

}
