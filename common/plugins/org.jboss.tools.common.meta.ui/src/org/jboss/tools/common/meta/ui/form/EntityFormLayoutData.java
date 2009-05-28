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

public class EntityFormLayoutData implements MetaConstants {

	final static IFormData CHILDREN_FOLDER_DEFINITION = new FormData(
		Messages.EntityFormLayoutData_ChildrenFolder,
		"", //"Description //$NON-NLS-1$
		"Children", //$NON-NLS-1$
		new FormAttributeData[]{new FormAttributeData("name", 100)}, //$NON-NLS-1$
		new String[]{CHILD_ENTITY},
		FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateChild") //$NON-NLS-1$
	);

	final static IFormData CHILDREN_LIST_DEFINITION = new FormData(
		Messages.EntityFormLayoutData_ChildrenList,
		"", //"Description //$NON-NLS-1$
		CHILDREN_ENTITY,
		new FormAttributeData[]{new FormAttributeData("name", 100)}, //$NON-NLS-1$
		new String[]{CHILD_ENTITY},
		FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateChild") //$NON-NLS-1$
	);

	private final static IFormData[] ENTITY_DEFINITIONS = new IFormData[] {
		new FormData(
			Messages.EntityFormLayoutData_Entity,
			"", //"Description //$NON-NLS-1$
			FormLayoutDataUtil.createGeneralFormAttributeData(ENTITY_ENTITY)
		),
		CHILDREN_FOLDER_DEFINITION,
		AttributeFormLayoutData.ATTRIBUTES_FOLDER_DEFINITION,
		new FormData(
			Messages.EntityFormLayoutData_AttributesFolder,
			"", //"Description //$NON-NLS-1$
			"ActionList", //$NON-NLS-1$
			new FormAttributeData[]{new FormAttributeData("name", 70), new FormAttributeData("element type", 30)}, //$NON-NLS-1$ //$NON-NLS-2$
			new String[]{ACTION_LIST_ENTITY, ACTION_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.AddItem") //$NON-NLS-1$
		),
		new FormData(
			Messages.EntityFormLayoutData_Advanced,
			"", //"Description //$NON-NLS-1$
			FormLayoutDataUtil.createAdvancedFormAttributeData(ENTITY_ENTITY)
		)
	};
	
	final static IFormData ENTITY_DEFINITION = new FormData(
		ENTITY_ENTITY, new String[]{null}, ENTITY_DEFINITIONS
	);

}
