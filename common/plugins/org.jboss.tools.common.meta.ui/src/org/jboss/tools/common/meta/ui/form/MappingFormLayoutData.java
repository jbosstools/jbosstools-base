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

public class MappingFormLayoutData implements MetaConstants {
	
	final static IFormData MAPPING_FOLDER_DEFINITION = new FormData(
		Messages.MappingFormLayoutData_MappingsFolder,
		"", //"Description //$NON-NLS-1$
		"Mappings", //$NON-NLS-1$
		new FormAttributeData[]{new FormAttributeData("name", 100)}, //$NON-NLS-1$
		new String[]{MAPPING_ENTITY},
		FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateMapping") //$NON-NLS-1$
	);

	final static IFormData MAPPING_LIST_DEFINITION = new FormData(
		Messages.MappingFormLayoutData_MappingsList,
		"", //"Description //$NON-NLS-1$
		MAPPINGS_ENTITY,
		new FormAttributeData[]{new FormAttributeData("name", 100)}, //$NON-NLS-1$
		new String[]{MAPPING_ENTITY},
		FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateMapping") //$NON-NLS-1$
	);

	private final static IFormData[] MAPPING_DEFINITIONS = new IFormData[] {
		new FormData(
			Messages.MappingFormLayoutData_MappingDefinitions,
			"", //"Description //$NON-NLS-1$
			FormLayoutDataUtil.createGeneralFormAttributeData(MAPPING_ENTITY)
		),
		new FormData(
			Messages.MappingFormLayoutData_Items,
			"", //"Description //$NON-NLS-1$
			new FormAttributeData[]{new FormAttributeData("name", 30), new FormAttributeData("value", 70)}, //$NON-NLS-1$ //$NON-NLS-2$
			new String[]{MAPPING_ITEM_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateItem") //$NON-NLS-1$
		)
	};

	final static IFormData MAPPING_DEFINITION = new FormData(
		MAPPING_ENTITY, new String[]{null}, MAPPING_DEFINITIONS
	);

}
