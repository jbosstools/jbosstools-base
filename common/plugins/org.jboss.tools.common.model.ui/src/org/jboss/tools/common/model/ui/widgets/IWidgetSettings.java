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
package org.jboss.tools.common.model.ui.widgets;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.forms.widgets.FormToolkit;

import org.jboss.tools.common.model.ui.widgets.border.Border;

public interface IWidgetSettings {
	public Object getObject (String key);
	public int    getStyle  (String key);
	public int    getInt    (String key);
	public Color  getColor  (String key);
	public Font   getFont   (String key);
	public Border getBorder (String key);
	public Cursor getCursor (String key);
	
	public void setupControl(Control control);

	public FormToolkit getToolkit(Display display);
}
