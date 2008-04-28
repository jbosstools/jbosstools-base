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

public class ActionListFormLayoutData implements MetaConstants {

	private final static IFormData[] ACTION_LIST_DEFINITIONS = new IFormData[] {
		new FormData(
			"Action List",
			"", //"Description
			FormLayoutDataUtil.createGeneralFormAttributeData(ACTION_LIST_ENTITY)
		),
		new FormData(
			"Items",
			"", //"Description
			new FormAttributeData[]{new FormAttributeData("name", 70), new FormAttributeData("element type", 30)},
			new String[]{ACTION_LIST_ENTITY, ACTION_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.AddItem")
		)
	};

	final static IFormData ACTION_LIST_DEFINITION = new FormData(
		ACTION_LIST_ENTITY, new String[]{null}, ACTION_LIST_DEFINITIONS
	);

	private final static IFormData[] ACTION_DEFINITIONS = new IFormData[] {
		new FormData(
			"Action",
			"", //"Description
			FormLayoutDataUtil.createGeneralFormAttributeData(ACTION_ENTITY)
		),
		new FormData(
			"Items",
			"", //"Description
			new FormAttributeData[]{new FormAttributeData("entity name", 70), new FormAttributeData("mandatory", 30)},
			new String[]{ENTITY_DATA_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateMetaEntityData")
		),
		new FormData(
			"Advanced",
			"", //"Description
			FormLayoutDataUtil.createAdvancedFormAttributeData(ACTION_ENTITY)
		),
	};

	final static IFormData ACTION_DEFINITION = new FormData(
		ACTION_ENTITY, new String[]{null}, ACTION_DEFINITIONS
	);

	private final static IFormData[] ENTITY_DATA_DEFINITIONS = new IFormData[] {
		new FormData(
			"Entity Data",
			"", //"Description
			FormLayoutDataUtil.createGeneralFormAttributeData(ENTITY_DATA_ENTITY)
		),
		new FormData(
			"Items",
			"", //"Description
			new FormAttributeData[]{new FormAttributeData("attribute name", 70), new FormAttributeData("mandatory", 30)},
			new String[]{ATTRIBUTE_DATA_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateMetaAttributeData")
		),
	};

	final static IFormData ENTITY_DATA_DEFINITION = new FormData(
		ENTITY_DATA_ENTITY, new String[]{null}, ENTITY_DATA_DEFINITIONS
	);

}
