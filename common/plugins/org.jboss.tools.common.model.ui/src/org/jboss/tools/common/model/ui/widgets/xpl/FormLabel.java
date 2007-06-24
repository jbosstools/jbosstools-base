/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Exadel, Inc.
 *     Red Hat, Inc. 
 *******************************************************************************/
package org.jboss.tools.common.model.ui.widgets.xpl;

import java.text.BreakIterator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.accessibility.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;

public class FormLabel extends Label {
	private String text = "";
	int marginWidth = 1;
	int marginHeight = 1;
	private boolean underlined;

	public FormLabel(Composite parent, int style) {
		super(parent, style);
		createPaintListener();
		initAccessible();
	}
	
	void createPaintListener() {
		addPaintListener(new PaintListener() {
			public void paintControl(PaintEvent e) {
				paint(e);
			}
		});
	}
	
	public String getText() {
		return text;
	}
	public void setText(String text) {
		this.text = (text != null) ? text : "";
	}

	protected void checkSubclass () {
		// empty
	}

	protected void initAccessible() {
		Accessible accessible = getAccessible();
		accessible.addAccessibleListener(new AccessibleAdapter() {
			public void getName(AccessibleEvent e) {
				e.result = getText();
			}

			public void getHelp(AccessibleEvent e) {
				e.result = getToolTipText();
			}
		});

		accessible
			.addAccessibleControlListener(new AccessibleControlAdapter() {
			public void getChildAtPoint(AccessibleControlEvent e) {
				Point pt = toControl(new Point(e.x, e.y));
				e.childID =	(getBounds().contains(pt)) ? ACC.CHILDID_SELF : ACC.CHILDID_NONE;
			}

			public void getLocation(AccessibleControlEvent e) {
				Rectangle location = getBounds();
				Point pt = toDisplay(new Point(location.x, location.y));
				e.x = pt.x;
				e.y = pt.y;
				e.width = location.width;
				e.height = location.height;
			}

			public void getChildCount(AccessibleControlEvent e) {
				e.detail = 0;
			}

			public void getRole(AccessibleControlEvent e) {
				e.detail = ACC.ROLE_LABEL;
			}

			public void getState(AccessibleControlEvent e) {
				e.detail = ACC.STATE_READONLY;
			}
		});
	}

	public void setUnderlined(boolean underlined) {
		this.underlined = underlined;
	}

	public boolean isUnderlined() {
		return underlined;
	}

	public Point computeSize(int wHint, int hHint, boolean changed) {
		int innerWidth = wHint;
		if (innerWidth != SWT.DEFAULT) innerWidth -= marginWidth * 2;
		Point textSize = computeTextSize(innerWidth, hHint);
		int textWidth = textSize.x + 2 * marginWidth;
		int textHeight = textSize.y + 2 * marginHeight;
		return new Point(textWidth, textHeight);
	}

	public static int computeWrapHeight(GC gc, String text, int width) {
		BreakIterator wb = BreakIterator.getWordInstance();
		wb.setText(text);
		FontMetrics fm = gc.getFontMetrics();
		int lineHeight = fm.getHeight();

		int saved = 0;
		int last = 0;
		int height = lineHeight;

		for (int loc = wb.first();
			loc != BreakIterator.DONE;
			loc = wb.next()) {
			String word = text.substring(saved, loc);
			Point extent = gc.textExtent(word);
			if (extent.x > width) {
				// overflow
				saved = last;
				height += extent.y;
			}
			last = loc;
		}
		return height;
	}

	private Point computeTextSize(int wHint, int hHint) {
		Point extent;
		GC gc = new GC(this);

		gc.setFont(getFont());
		if ((getStyle() & SWT.WRAP) != 0 && wHint != SWT.DEFAULT) {
			int height = computeWrapHeight(gc, text, wHint);
			extent = new Point(wHint, height);
		} else {
			extent = gc.textExtent(getText());
		}
		gc.dispose();
		return extent;
	}

	public static void paintWrapText(
		GC gc,
		Point size,
		String text,
		int marginWidth,
		int marginHeight) {
		paintWrapText(gc, size, text, marginWidth, marginHeight, false);
	}

	public static void paintWrapText(GC gc,	Point size,	String text, int marginWidth, int marginHeight, boolean underline) {
		BreakIterator wb = BreakIterator.getWordInstance();
		wb.setText(text);
		FontMetrics fm = gc.getFontMetrics();
		int lineHeight = fm.getHeight();
		int descent = fm.getDescent();

		int saved = 0;
		int last = 0;
		int y = marginHeight;
		int width = size.x - marginWidth * 2;

		for (int loc = wb.first();
			loc != BreakIterator.DONE;
			loc = wb.next()) {
			String line = text.substring(saved, loc);
			Point extent = gc.textExtent(line);
			if (extent.x > width) {
				// overflow
				String prevLine = text.substring(saved, last);
				gc.drawString(prevLine, marginWidth, y, true);
				if (underline) {
					Point prevExtent = gc.textExtent(prevLine);
					int lineY = y + lineHeight - descent + 1;
					gc.drawLine(marginWidth, lineY, prevExtent.x, lineY);
				}

				saved = last;
				y += lineHeight;
			}
			last = loc;
		}
		String lastLine = text.substring(saved, last);
		gc.drawString(lastLine, marginWidth, y, true);
		if (underline) {
			int lineY = y + lineHeight - descent + 1;
			Point lastExtent = gc.textExtent(lastLine);
			gc.drawLine(marginWidth, lineY, marginWidth + lastExtent.x, lineY);
		}
	}

	protected void paint(PaintEvent e) {
		Point s = getSize();
		e.gc.setFont(getFont());
		e.gc.setForeground(getForeground());
		if ((getStyle() & SWT.WRAP) != 0) {
			paintWrapText(e.gc, s, text, marginWidth, marginHeight, underlined);
		} else {
			e.gc.drawText(getText(), marginWidth, marginHeight, true);
			if (underlined) underline(e.gc, s);
		}
	}
	
	private void underline(GC gc, Point s) {
		FontMetrics fm = gc.getFontMetrics();
		int descent = fm.getDescent();
		int lineY = s.y - marginHeight - descent + 1;
		gc.drawLine(marginWidth, lineY,	s.x - marginWidth, lineY);
	}

}
