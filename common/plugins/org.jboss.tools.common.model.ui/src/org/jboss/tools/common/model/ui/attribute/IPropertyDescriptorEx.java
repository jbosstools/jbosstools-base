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
package org.jboss.tools.common.model.ui.attribute;

import org.eclipse.core.runtime.IAdaptable;
import org.jboss.tools.common.model.ui.attribute.editor.PropertyEditor;
import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.IPropertySource;

public interface IPropertyDescriptorEx extends IAdaptable, 
												IPropertyDescriptor {
	public PropertyEditor getPropertyEditor();
	public IPropertySource getPropertySource();
}
