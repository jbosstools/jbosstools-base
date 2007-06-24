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

import java.util.ArrayList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class TextAndReferenceComponent extends Canvas implements PaintListener, MouseMoveListener {
	int defaultWidth = 100;
	int width;
	String text = "";
	Line[] lines = new Line[0];
	int lineHeight = 12;
	ReferenceListener listener;
	Font plain;
	Font bold;
	
	Token[] tokens = new Token[0]; 
	
	public TextAndReferenceComponent(Composite parent, int style) {
		super (parent, style);
		addPaintListener(this);
		addMouseMoveListener(this);
		addMouseListener(new MA());
		try {
			plain = getFont();
			FontData d = plain.getFontData()[0];
			bold = new Font(null, d.getName(), d.getHeight(), d.getStyle() | SWT.BOLD);
		} catch (Exception e) {
			ModelUIPlugin.log(e);
		}
	}

	public void mouseMove(MouseEvent e) {
		int c = (overReference(e.x, e.y)) ? SWT.CURSOR_HAND : SWT.CURSOR_ARROW;
		setCursor(new Cursor(Display.getCurrent(), c));
	}
	
	private boolean overReference(int x, int y) {
		Token t = findToken(x, y);
		return t != null && t.isReference;
	}
		
	public Point computeSize(int wHint, int hHint, boolean changed) {
		width = (wHint == SWT.DEFAULT) ? defaultWidth : wHint;
		GC g = new GC(this);
		g.setFont(getFont());
		lines = Tokenizer.breakIntoLines(g, tokens, width);
		return new Point(width + 10, lines.length * lineHeight + 6);
	}
		
	public int getStringWidth(Font font, String str){
		GC g = new GC(this);
		int size = getStringWidth(g, font, str);
		g.dispose();
		return size;
	} 
	
	public static int getStringWidth(GC g, Font font, String str){
		int size = 0;
		g.setFont(font);
		for(int i = 0; i < str.length(); i++) {
			char c = str.charAt(i);
			size += g.getCharWidth(c) + 1;
			if(c == ' ') size += 1;
		}
		return size;
	}
	
	public void setText(String text, int width) {
		this.text = text;
		defaultWidth = width;
		this.width = width;
		tokens = Tokenizer.tokenize(text, plain, bold);
		try {
			pack();
		} catch (Exception e) {
			ModelUIPlugin.log(e);
		}
	}
	
	public void paintControl(PaintEvent ev) {
		if(lines.length == 0) return;
		GC g = ev.gc;
		Font f = getFont();
		g.setFont(f);
		for (int i = 0; i < lines.length; i++) {
			int y = 2 + i * lineHeight;
			lines[i].paint(tokens, g, y);
		}
	}
	
	public void addReferenceListener(ReferenceListener listener) {
		this.listener = listener;
	}
	
	class MA extends MouseAdapter {
		public void mouseDown(MouseEvent e) {
			Token t = findToken(e.x, e.y);
			if(t != null && listener != null && t.isReference) {
				listener.referenceSelected(t.text);
			}
		}
	}
	
	Token findToken(int x, int y) {
		for (int i = 0; i < tokens.length; i++)
			if(tokens[i].contains(x, y)) return tokens[i];
		return null;		
	}
		
}

class Context {
	ArrayList<Token> l = new ArrayList<Token>();
	StringBuffer word = new StringBuffer();
	boolean isReference = false;
	boolean isBold = false;
	String text;
	int i = -1;
	char c = '\0';
	Font plain;
	Font bold;
	
	boolean nextChar() {
		++i;
		if(i >= text.length()) return false;
		c = text.charAt(i);
		return true;
	}
	
	void appendToWord() {
		word.append(c);
	}
	
	boolean isReferenceStart() {
		boolean b = (c == '<' && text.indexOf("<a>", i) == i && !isReference);
		if(b) {
			getLastToken();
			isReference = true; 
			i += 2;
		}
		return b;
	}
	
	boolean isReferenceEnd() {
		boolean b = (c == '<' && text.indexOf("</a>", i) == i && isReference);
		if(b) {
			getLastToken();
			isReference = false; 
			i += 3;
		}
		return b;
	}
	
	boolean isBoldStart() {
		boolean b = (c == '<' && text.indexOf("<b>", i) == i && !isBold);
		if(b) {
			getLastToken();
			isBold = true; 
			i += 2;
		}
		return b;
	}
	
	boolean isBoldEnd() {
		boolean b = (c == '<' && text.indexOf("</b>", i) == i && isBold);
		if(b) {
			getLastToken();
			isBold = false; 
			i += 3;
		}
		return b;
	}
	
	boolean isWordEnd() {
		boolean b = (c == ' ');
		if(b) {
			getLastToken();
		}
		return b;
	}
	
	boolean isBreakLine() {
		boolean b = (c == '\n');
		if(b) {
			getLastToken();
			l.add(Token.createBreakToken());
		}
		return b;
	}
	
	String readLastWord() {
		String w = word.toString();
		word.setLength(0);
		return w;
	}
	
	Token getLastToken() {
		String w = readLastWord();
		if(w.length() == 0) return null;
		Token t = Token.createToken(w, isReference, (isBold) ? bold : plain);
		l.add(t);
		return t;
	}
	
}

class Tokenizer {
	public static Token[] tokenize(String text, Font plain, Font bold) {
		Context context = new Context();
		context.bold = bold;
		context.plain = plain;
		context.text = text;
		while(context.nextChar()) {
			if(context.isBreakLine()) {
			} else if(context.isWordEnd()) {
			} else if(context.isReferenceStart()) {
			} else if(context.isReferenceEnd()) {
			} else if(context.isBoldStart()) {
			} else if(context.isBoldEnd()) {
			} else {
				context.appendToWord();
			}
		}
		context.getLastToken();
		return (Token[])context.l.toArray(new Token[0]);
	}
    
	public static Line[] breakIntoLines(GC g, Token[] tokens, int width) {
		ArrayList<Line> l = new ArrayList<Line>();
		Line line = null;
		int x = 0;
		for (int i = 0; i < tokens.length; i++) {
			if(line == null) {
				line = new Line();
				line.firstToken = i;
				line.lastToken = i;
				x = 0;
			}
			if(tokens[i].isBreak) {
				l.add(line);
				line = null;
			} else {
				int dx = TextAndReferenceComponent.getStringWidth(g, tokens[i].font, tokens[i].text);
				if(x > 0 && x + 5 + dx > width) {
					l.add(line);
					line = null;
					--i;
				} else {
					line.lastToken = i;
					if(x == 0) x = dx; else x = x + 5 + dx;
				}
			}
		}
		if(line != null) l.add(line);    	
		return (Line[])l.toArray(new Line[0]);    	
	}
}

class Line {
	int firstToken = -1;
	int lastToken = -1;

	public void paint(Token[] tokens, GC gc, int y) {
		int x = 0;
		for (int i = firstToken; i <= lastToken && i < tokens.length; i++) {
			x = tokens[i].paint(gc, y, x) + 4;
		}
	}
    
}

class Token {
	String text = "";
	boolean isReference = false;
	boolean isBreak = false;
	Rectangle r = new Rectangle(0, 0, 0, 0);
	Font font;
	
	public int paint(GC g, int y, int x) {
		if(isBreak) return x;
		g.setFont(font);
		r.x = x - 2;
		r.y = y;
		int color = (isReference) ? SWT.COLOR_BLUE : SWT.COLOR_BLACK;
		g.setForeground(Display.getDefault().getSystemColor(color));
		g.drawString(text, x, y);
		int w = TextAndReferenceComponent.getStringWidth(g, g.getFont(), text);
		int x1 = x;
		int x2 = x1 + w + 2;
		int y1 = y + g.getFont().getFontData()[0].getHeight() + 4;
		if(isReference) g.drawLine(x1 - 2, y1, x2, y1);
		r.x = x1;
		r.y = y;
		r.width = x2 - x1 + 2;
		r.height = y1 - y;		
		return x1 + w;
	}
	
	public boolean contains(int x, int y) {
		return x >= r.x && x <= r.x + r.width && y >= r.y && y < r.y + r.height;
	}
	
	public static Token createBreakToken() {
		Token t = new Token();
		t.isBreak = true;
		return t;
	}

	public static Token createToken(String text, boolean reference, Font font) {
		Token t = new Token();
		t.text = text;
		t.isReference = reference;
		t.font = font;
		return t;
	}

}
