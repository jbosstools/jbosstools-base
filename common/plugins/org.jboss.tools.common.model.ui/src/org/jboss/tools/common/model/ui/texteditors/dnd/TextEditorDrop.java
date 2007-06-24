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
package org.jboss.tools.common.model.ui.texteditors.dnd;

import java.util.Properties;

import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.graphics.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.dnd.*;
import org.eclipse.jface.text.source.ISourceViewer;

import org.jboss.tools.common.model.XModelObject;

public class TextEditorDrop implements IControlDragDropProvider, IControlDropListener {
	public interface TextEditorDropProvider2 {
		public String getContext(int pos);
	}
	ControlDragDrop dnd = new ControlDragDrop();
	
	TextEditorDropProvider provider;
	
	public TextEditorDrop() {
		dnd.setProvider(this);
	}
	
	public void setTextEditorDropProvider(TextEditorDropProvider provider) {
		this.provider = provider;
	}
	
	public void enable() {
		dnd.enableDrop();
	}

	public Control getControl() {
		return provider.getSourceViewer().getTextWidget(); 
	}

	public XModelObject getModelObjectForWidget(Widget widget) {
		return provider.getModelObject();
	}

	public Widget[] getSelection() {
		return new Widget[]{getControl()};
	}

	public Properties getDropProperties(int x, int y) {
		Properties p = new Properties();
		p.setProperty("isDrop", "true");
		p.setProperty("actionSourceGUIComponentID", "editor");
		p.setProperty("accepsAsString", "true");
		String text = provider.getSourceViewer().getDocument().get();
		p.setProperty("text", text);
		int pos = getPosition(x, y);
		p.setProperty("pos", "" + pos);
		p.put("viewer", provider.getSourceViewer());
		if(provider instanceof TextEditorDropProvider2) {
			String context = ((TextEditorDropProvider2)provider).getContext(pos);
			if(context != null) {
				p.put("text-context", context);
			}
		}
		return p;
	}
	
	private int getPosition(int x, int y) {
		try {
			ISourceViewer v = provider.getSourceViewer();
			StyledText t = v.getTextWidget();
			Point pp = t.toControl(x, y);
			x = pp.x;
			y = pp.y;		
			int lineIndex = (t.getTopPixel() + y) / t.getLineHeight();
			if (lineIndex >= t.getLineCount()) {
				return t.getCharCount();
			} else {
				int c = 0;
				try {
					c = t.getOffsetAtLocation(new Point(x, y));
					if(c < 0) c = 0;
				} catch (Exception ex) {
                    c = t.getOffsetAtLine(lineIndex + 1) - 
                    (t.getLineDelimiter() == null ? 0 : t.getLineDelimiter().length());					
				}
				return c;
			}
		} catch (Exception e) {
			return 0;
		}
	}

	public void drop(Properties p) {
		try {
			ISourceViewer v = provider.getSourceViewer();
			int x = ((Integer)p.get("drop.x")).intValue();
			int y = ((Integer)p.get("drop.y")).intValue();
			int c = getPosition(x, y);		
			Point sp = v.getTextWidget().getSelectionRange();
			if(sp == null || c < sp.x || c >= sp.x + sp.y) {
				v.getTextWidget().setCaretOffset(c);
			}
			provider.insert(p);
		} catch (Exception e) {
			ModelUIPlugin.log(e);
		}
	}
	
}
