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
package org.jboss.tools.common.editor;

import java.util.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.XModelObject;

public class ErrorMode {
	private ScrolledComposite errors = null;
	Composite c = null;
	private ArrayList<Lbl> labels = new ArrayList<Lbl>();
	String es = "";
	ErrorSelectionListener listener;
	boolean visible = true;
	
	public void addErrorSelectionListener(ErrorSelectionListener listener) {
		this.listener = listener;
		for (int i = 0; i < labels.size(); i++) {
			Lbl l = (Lbl)labels.get(i);
			l.listener = listener;
		}
	}
	
	public void setEnabled(boolean b) {
		if(visible == b) return;
		visible = b;
		if(errors != null && !errors.isDisposed()) errors.setVisible(b);
	}
	
	public boolean isVisible() {
		return visible;
	}
	
	public boolean isEnabled() {
		return (errors != null && errors.isVisible());
	}
	
	public boolean isDisposed() {
		return errors == null || errors.isDisposed();
	}

	public void dispose() {
		if(errors != null) {
			try { errors.dispose(); } catch (Exception e) {}
			errors = null;
			labels.clear();
			es = "";
		}
		listener = null;
		c = null;
	}

	public Control createControl(Composite composite) {
		errors = new ScrolledComposite(composite, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
		errors.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_WHITE));
		return errors;
	}
	
	public Control getControl() {
		return errors;
	}
	
	private void createC() {
		if(c != null && !c.isDisposed()) c.dispose();
		c = new Composite(errors, SWT.NONE);
		errors.setContent(c);
		GridLayout l = new GridLayout();
		l.verticalSpacing = 0;
		l.marginWidth = 0;
		c.setLayout(l);
		c.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_WHITE));
	}

	public void update(XModelObject o) {
		update(o.get("errors"));
	}
	public void update(String err) {
		String s = err;
		if(s == null) s = "";
		if(es.equals(s)) return;
		    createC();
		    labels.clear();
		String[] el = getErrors(s);
		for (int i = 0; i < el.length; i++) {
			Lbl lb = null;
			if(i < labels.size()) {
				lb = (Lbl)labels.get(i);
			} else {
				lb = new Lbl();
				lb.listener = listener;
				labels.add(lb);
			}
			lb.update(el[i], c);
		}
		for (int i = labels.size() - 1; i >= el.length; i--) {
			Lbl lb = (Lbl)labels.get(i);
			lb.dispose();
			labels.remove(i);
		}
		errors.getParent().update();
		errors.getParent().layout();
		c.setSize(c.computeSize(SWT.DEFAULT, SWT.DEFAULT));
		c.update();
		c.layout();
	}
	
	String[] getErrors(String s) {
		if(s == null) s = "";
		StringTokenizer st = new StringTokenizer(s, "\n");
		String[] rs = new String[st.countTokens()];
		for (int i = 0; i < rs.length; i++) rs[i] = st.nextToken();
		return rs;		
	}
	
	public void go() {
		if(labels.size() == 0) return;
		Lbl lb = (Lbl)labels.get(0);
		lb.go();
	}
	
}

class Lbl {
	private L label = null; 
	private String error = "";
	private String[] messages = new String[]{"", "", ""};
	private int line = 1;
	private int position = 1; 
	ErrorSelectionListener listener;

	public void update(String s, Composite parent) {
		if(label == null || label.isDisposed()) {
			Composite c = new Composite(parent, SWT.NONE);
			c.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			GridLayout l = new GridLayout();
			l.marginWidth = 0;
			c.setLayout(l);
			c.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_WHITE));
			label = new L(c, SWT.NONE);
			label.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_WHITE));
			label.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			label.addMouseListener(new SL());
			label.setData(messages);
		}
		if(s == null) s = "";
		if(!error.equals(s)) parse(s);
	}
	
	private void parse(String s) {
		error = s;
		line = 1; position = 1;
		int i = s.indexOf('@');
		int j = s.indexOf('@', i + 1);
		int k = s.indexOf('@', j + 1);
		if(k > j) {
			String q = s.substring(j + 1, k);
			messages[0] = s.substring(0, i);
			messages[1] =  q;
			messages[2] = s.substring(k + 1);
			int qi = q.indexOf(':');
			if(qi > 0) {
				try { line = Integer.parseInt(q.substring(0, qi)); } catch (Exception e) {}
				try { position = Integer.parseInt(q.substring(qi + 1)); } catch (Exception e) {}
			}
		} else {
			messages[0] = "ERROR";
			messages[1] = "0:0";
			messages[2] = s;
		}
	}
	
	public void go() {
		if(listener != null) listener.errorSelected(line, position);
	}
	
	class SL extends MouseAdapter {
		public void mouseUp(MouseEvent e) {
			if(label.isLink(e.x, e.y)) go();
		}
	}
	
	public void dispose() {
		if(label != null && !label.isDisposed()) label.dispose();
	}
	
}

class L extends Canvas implements PaintListener, MouseMoveListener {
	Cursor DEF_CURSOR = new Cursor(null, SWT.CURSOR_ARROW);
	Cursor HAND_CURSOR = new Cursor(null, SWT.CURSOR_HAND);
	private String[] messages = new String[]{"", "", ""};
		
	public L(Composite parent, int style) {
		super (parent, style);
		addPaintListener(this);
		addMouseMoveListener(this);
	}
		
	public void setData(String[] messages) {
		this.messages = messages;
	}

	public Point computeSize (int wHint, int hHint, boolean changed) {
		int w0 = getStringWidth(getFont(), messages[0]);
		int w1 = getStringWidth(getFont(), messages[1]);
		int w2 = getStringWidth(getFont(), messages[2]);
		return new Point(2 + w0 + 10 + w1 + 10 + w2 + 10, getLineHeight() + 6);
	}
		
	public int getStringWidth(Font font, String str){
		int size = 0;
		GC g = new GC(this);
		g.setFont(font);
		for(int i = 0; i < str.length(); i++) {
			size += g.getCharWidth(str.charAt(i)) + 1;
		}
		g.dispose();
		return size;
	}
	
	public int getLineHeight() {
		try { 
			return getFont().getFontData()[0].getHeight() + 2; 
		} catch (Exception e) { 
			return 20; 
		}
	}
		
	public void paintControl(PaintEvent ev) {
	   GC g = ev.gc;
	   Font f = getFont();
	   g.setFont(f);
	   int w0 = getStringWidth(f, messages[0]);
	   int w1 = getStringWidth(f, messages[1]);
//	   int w2 = getStringWidth(f, messages[2]);
	   int x = 2;
	   g.setForeground(Display.getDefault().getSystemColor(SWT.COLOR_RED));
	   g.drawString(messages[0], x, 2);
	   x += w0 + 10;
	   g.setForeground(Display.getDefault().getSystemColor(SWT.COLOR_BLUE));
	   g.drawString(messages[1], x, 2);
	   int h = getLineHeight() + 4;
	   g.drawLine(x, h, x + w1 + 2, h);
	   x += w1 + 10;
	   g.setForeground(Display.getDefault().getSystemColor(SWT.COLOR_BLACK));
	   g.drawString(messages[2], x, 2);
	}
		
	public boolean isLink(int x, int y) {
		Font f = getFont();
		int w0 = getStringWidth(f, messages[0]);
		int w1 = getStringWidth(f, messages[1]);
		int x1 = 2 + w0 + 10, x2 = x1 + w1;
		return (x > x1 && x < x2);
	}

	public void mouseMove(MouseEvent e) {
		boolean l = isLink(e.x, e.y);
		Cursor c = (l) ? HAND_CURSOR : DEF_CURSOR;
		setCursor(c);
	}

}
	