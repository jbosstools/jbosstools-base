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

public class IconFormLayoutData implements MetaConstants {

	private final static IFormData[] ICON_GROUP_DEFINITIONS = new IFormData[] {
		new FormData(
			Messages.IconFormLayoutData_IconGroup,
			"", //"Description //$NON-NLS-1$
			FormLayoutDataUtil.createGeneralFormAttributeData(ICON_GROUP_ENTITY)
		),
		new FormData(
			Messages.IconFormLayoutData_Subgroups,
			"", //"Description //$NON-NLS-1$
			new FormAttributeData[]{new FormAttributeData("name", 100)}, //$NON-NLS-1$
			new String[]{ICON_GROUP_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateIconGroup") //$NON-NLS-1$
		),
		new FormData(
			Messages.IconFormLayoutData_Icons,
			"", //"Description //$NON-NLS-1$
			new FormAttributeData[]{new FormAttributeData("name", 30), new FormAttributeData("path", 70)}, //$NON-NLS-1$ //$NON-NLS-2$
			new String[]{ICON_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateIcon") //$NON-NLS-1$
		)
	};

	final static IFormData ICON_GROUP_DEFINITION = new FormData(
		ICON_GROUP_ENTITY, new String[]{null}, ICON_GROUP_DEFINITIONS
	);

	private final static IFormData[] ICONS_DEFINITIONS = new IFormData[] {
		new FormData(
			Messages.IconFormLayoutData_IconsDefinitions,
			"", //"Description //$NON-NLS-1$
			new FormAttributeData[]{new FormAttributeData("name", 100)}, //$NON-NLS-1$
			new String[]{ICON_GROUP_ENTITY},
			FormLayoutDataUtil.createDefaultFormActionData("CreateActions.CreateIconGroup") //$NON-NLS-1$
		),
	};

	final static IFormData ICONS_DEFINITION = new FormData(
		ICONS_ENTITY, new String[]{null}, ICONS_DEFINITIONS
	);


}
