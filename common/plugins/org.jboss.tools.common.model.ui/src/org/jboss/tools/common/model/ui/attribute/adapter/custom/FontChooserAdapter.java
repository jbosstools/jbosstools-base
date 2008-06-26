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
package org.jboss.tools.common.model.ui.attribute.adapter.custom;

import org.jboss.tools.common.model.ui.*;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.*;

public class FontChooserAdapter extends DefaultValueAdapter implements IActionHelper {	
	
	public String invoke(Control control) {
		return invoke0(control);			
	}
	
	public String getCommand() {
		return "...";
	}
	
	public String invoke0(Control control) {
		FontDialog dialog = new FontDialog(control.getShell());
		String v = "" + getValue();
		ColoredFontData data = ColoredFontData.toFontData(v);
		dialog.setFontList(new FontData[]{data.getFontData()});
		dialog.setRGB(data.getColor());
		FontData font = dialog.open();
		if(font == null) return null;
		return (font == null) ? null : ColoredFontData.toString(font, dialog.getRGB());
	}
	
	public Object getAdapter(Class adapter) {
		if (adapter == IActionHelper.class) return this;
		return super.getAdapter(adapter);
	}
	
}
