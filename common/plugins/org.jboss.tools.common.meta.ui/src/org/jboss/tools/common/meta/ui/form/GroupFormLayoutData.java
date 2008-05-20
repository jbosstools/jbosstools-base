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

public class GroupFormLayoutData implements MetaConstants {

	private final static IFormData[] GROUP_DEFINITIONS = new IFormData[] {
		new FormData(
			"Meta Group",
			"", //"Description
			FormLayoutDataUtil.createGeneralFormAttributeData(GROUP_ENTITY)
		),
		new FormData(
			"Entities",
			"", //"Description
			new FormAttributeData[]{new FormAttributeData("name", 100)},
			new String[]{ENTITY_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateEntity")
		),
		new FormData(
			"Extensions",
			"", //"Description
			new FormAttributeData[]{new FormAttributeData("name", 100)},
			new String[]{EXTENSION_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateEntityExtension")
		),
		MappingFormLayoutData.MAPPING_FOLDER_DEFINITION
	};

	final static IFormData GROUP_DEFINITION = new FormData(
		GROUP_ENTITY, new String[]{null}, GROUP_DEFINITIONS
	);

}
