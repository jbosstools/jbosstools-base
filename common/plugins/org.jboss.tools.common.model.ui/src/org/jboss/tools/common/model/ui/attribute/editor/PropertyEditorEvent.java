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
package org.jboss.tools.common.model.ui.attribute.editor;

import java.util.EventObject;

public class PropertyEditorEvent extends EventObject {
	private static final long serialVersionUID = 4781898482413679653L;

	public PropertyEditorEvent(Object source) {
		super(source);
	}
	
	public IPropertyEditor getPropertyEditor() {
		return (IPropertyEditor)source;
	}
}
