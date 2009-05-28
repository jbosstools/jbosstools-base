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

import org.jboss.tools.common.meta.ui.Messages;
import org.jboss.tools.common.model.ui.forms.FormAttributeData;
import org.jboss.tools.common.model.ui.forms.FormData;
import org.jboss.tools.common.model.ui.forms.FormLayoutDataUtil;
import org.jboss.tools.common.model.ui.forms.IFormData;

public class AttributeFormLayoutData implements MetaConstants {

	private final static IFormData[] ATTRIBUTE_DEFINITIONS = new IFormData[] {
		new FormData(
			Messages.AttributeFormLayoutData_Attribute,
			"", //"Description //$NON-NLS-1$
			FormLayoutDataUtil.createGeneralFormAttributeData(ATTRIBUTE_ENTITY)
		),
		new FormData(
			Messages.AttributeFormLayoutData_Constraint,
			"", //"Description //$NON-NLS-1$
			"Constraint", //$NON-NLS-1$
			FormLayoutDataUtil.createGeneralFormAttributeData(ATTRIBUTE_CONSTRAINT_ENTITY)
		),
		new FormData(
			Messages.AttributeFormLayoutData_Editor,
			"", //"Description //$NON-NLS-1$
			"Editor", //$NON-NLS-1$
			FormLayoutDataUtil.createGeneralFormAttributeData(ATTRIBUTE_EDITOR_ENTITY)
		),
		new FormData(
			Messages.AttributeFormLayoutData_Advanced,
			"", //"Description //$NON-NLS-1$
			FormLayoutDataUtil.createAdvancedFormAttributeData(ATTRIBUTE_ENTITY)
		),
	};

	private final static IFormData[] ATTRIBUTE_REF_DEFINITIONS = new IFormData[] {
		new FormData(
			Messages.AttributeFormLayoutData_AttributeReference,
			"", //"Description //$NON-NLS-1$
			FormLayoutDataUtil.createGeneralFormAttributeData(ATTRIBUTE_REF_ENTITY)
		),
	};

	final static IFormData ATTRIBUTES_FOLDER_DEFINITION = new FormData(
		Messages.AttributeFormLayoutData_AttributesFolder,
		"", //"Description, //$NON-NLS-1$
		"Attributes", //$NON-NLS-1$
		new FormAttributeData[]{new FormAttributeData("name", 100)}, //$NON-NLS-1$
		new String[]{ATTRIBUTE_ENTITY,ATTRIBUTE_REF_ENTITY},
		FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateAttribute") //$NON-NLS-1$
	);

	final static IFormData ATTRIBUTES_LIST_DEFINITION = new FormData(
		Messages.AttributeFormLayoutData_AttributesList,
		"", //"Description, //$NON-NLS-1$
		ATTRIBUTES_ENTITY,
		new FormAttributeData[]{new FormAttributeData("name", 100)}, //$NON-NLS-1$
		new String[]{ATTRIBUTE_ENTITY,ATTRIBUTE_REF_ENTITY},
		FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateAttribute") //$NON-NLS-1$
	);

	final static IFormData ATTRIBUTE_DEFINITION = new FormData(
		ATTRIBUTE_ENTITY, new String[]{null}, ATTRIBUTE_DEFINITIONS
	);

	final static IFormData ATTRIBUTE_REF_DEFINITION = new FormData(
		ATTRIBUTE_REF_ENTITY, new String[]{null}, ATTRIBUTE_REF_DEFINITIONS
	);

	private final static IFormData[] ATTRIBUTE_CONSTRAINT_DEFINITIONS = new IFormData[] {
		new FormData(
			Messages.AttributeFormLayoutData_ConstraintDefinitions,
			"", //"Description //$NON-NLS-1$
			FormLayoutDataUtil.createGeneralFormAttributeData(ATTRIBUTE_CONSTRAINT_ENTITY)
		),
		new FormData(
			Messages.AttributeFormLayoutData_Items,
			"", //"Description //$NON-NLS-1$
			new FormAttributeData[]{new FormAttributeData("name", 100)}, //$NON-NLS-1$
			new String[]{CONSTRAINT_ITEM_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateConstraintItem") //$NON-NLS-1$
		),
	};

	final static IFormData ATTRIBUTE_CONSTRAINT_DEFINITION = new FormData(
		ATTRIBUTE_CONSTRAINT_ENTITY, new String[]{null}, ATTRIBUTE_CONSTRAINT_DEFINITIONS
	);

}
