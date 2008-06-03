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

public class ExtensionFormLayoutData implements MetaConstants {

	private final static IFormData[] EXTENSION_DEFINITIONS = new IFormData[] {
		new FormData(
			"Entity Extension",
			"", //"Description
			FormLayoutDataUtil.createGeneralFormAttributeData(EXTENSION_ENTITY)
		),
		EntityFormLayoutData.CHILDREN_FOLDER_DEFINITION,
		new FormData(
			"Action List",
			"", //"Description
			"ActionList",
			new FormAttributeData[]{new FormAttributeData("name", 70), new FormAttributeData("element type", 30)},
			new String[]{ACTION_LIST_ENTITY, ACTION_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.AddItem")
		)
	};

	final static IFormData EXTENSION_DEFINITION = new FormData(
			EXTENSION_ENTITY, new String[]{null}, EXTENSION_DEFINITIONS
	);

}
