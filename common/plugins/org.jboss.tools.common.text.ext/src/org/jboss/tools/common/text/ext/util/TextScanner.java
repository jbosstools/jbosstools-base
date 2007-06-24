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
package org.jboss.tools.common.text.ext.util;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.rules.IWhitespaceDetector;
import org.eclipse.jface.text.rules.IWordDetector;
import org.eclipse.jface.text.rules.Token;

/**
 * @author Jeremy
 *
 */
public abstract class TextScanner implements ITokenScanner {
	protected Reader reader;
	protected InputStream stream;
	
	protected int offset;
	protected int length;
	protected int position;
	private StringBuffer text = new StringBuffer();

	public TextScanner(InputStream stream) {
		this.stream = stream;
		this.reader = new BufferedReader(new InputStreamReader(stream));
	}
	
	public TextScanner(Reader reader) {
		this.stream = null;
		this.reader = reader;
	}

	protected void finalize() throws Exception {
		if (text != null) text.setLength(0);
		text = null;
	}

	public void setRange(IDocument document, int offset, int length) {;}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.rules.ITokenScanner#nextToken()
	 */
	abstract public IToken nextToken() ;

	/*
	 * @see org.eclipse.jface.text.rules.ITokenScanner#getTokenOffset()
	 */
	public int getTokenOffset() {
		return offset;
	}

	/*
	 * @see org.eclipse.jface.text.rules.ITokenScanner#getTokenLength()
	 */
	public int getTokenLength() {
		return length;
	}

	public static class TextToken extends Token {
		private String text ;
		
		public TextToken(String type, String text) {
			super(type);
			this.text = text;
		}
		public String getText() {
			return text;
		}
		public String getType() {
			return (String)getData();
		}
		public boolean equals (Object obj) {
			if (!(obj instanceof TextToken)) return false;
			TextToken tt = (TextToken)obj;
			if (getType() == null) {
				if (tt.getType() != null) return false;
			} else {
				if (!getType().equals(tt.getType())) return false;
			}
			if (getText() == null) {
				if (tt.getText() != null) return false;
			} else {
				if (!getText().equals(tt.getText())) return false;
			}
			return true;
		}
	}
	
	protected IToken getToken( String type ) {
		length = position - offset;

		if ( length == 0 ) return Token.EOF;
		if ( type == null ) return Token.UNDEFINED;

//		if (tokens == null) tokens = new HashMap();
//		IToken token = (IToken) tokens.get( type );
//		if ( token == null ) {
//			token = new TextToken( type, text.substring(offset, offset + length) );
//			tokens.put( type, token );
//		}

//		return token;
		return new TextToken( type, text.substring(offset, offset + length) );
	}


	protected int read() {
		int ch = -1;
		if (position < text.length()) {
			ch = text.charAt(position);
		} else {
			try {
				ch = reader.read();
			} catch (Exception x) {
				return ICharacterScanner.EOF;
			}
			if ( ch == -1 ) return ICharacterScanner.EOF;
			this.text.append((char)ch);
		}
		position++;
		return ch;
	}

	protected void unread() { if (position > 0) --position; }

	static class DecimalTokenDetector implements IWordDetector {
		public boolean isWordStart(char c) {
			return isWordPart(c);
		}

		public boolean isWordPart(char c) {
			switch (c){
				case '0': case '1': case '2': case '3': case '4':
				case '5': case '6': case '7': case '8': case '9':
					return true;
			}
			return false;
		}
	}

	static class HexadecimalTokenDetector extends DecimalTokenDetector {
		public boolean isWordStart(char c) {
			return (c == 'x');
		}

		public boolean isWordPart(char c) {
			if (super.isWordPart(c)) return true;
			switch (c){
				case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
				case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
					return true;
			}
			return false;
		}
	}

	public static final IWhitespaceDetector WS_DETECTOR = new WhitespaceDetector();
	public static final IWordDetector NMTOKEN_DETECTOR = new NameTokenDetector();
	public static final IWordDetector DECIMAL_DETECTOR = new DecimalTokenDetector();
	public static final IWordDetector HEXADECIMAL_DETECTOR = new HexadecimalTokenDetector();

	public int skipWhitespaceToken() {
		int ch = read();
		int readsCount = 0;
		while (ch != ICharacterScanner.EOF) {
			if (!WS_DETECTOR.isWhitespace((char)ch)) {
				unread();
				return readsCount;
			}
			readsCount++;
			ch = read();
		}
		return readsCount;
	}

	protected void clearText() {
		if (this.text == null) this.text = new StringBuffer();
		this.text.setLength(0);
		this.offset = 0;
		this.position = 0;
		this.length = 0;
	}
	
	public static int calcTokenLength(char[] tokenChars, IDocument d, int start, int stop) {
		int count = 0;
		while (start + count < stop && count < tokenChars.length) {
			try {
				if (d.getChar(start+count) != tokenChars[count]) return 0;
			} catch (BadLocationException ex) {
				return 0;
			}
			count++;
		}
		return count;
	}

	public static int calcTokenLength(IWordDetector wd, IDocument d, int start, int stop) {
		int count = 0;
		try {
			if (!wd.isWordStart(d.getChar(start)))
				return 0;
			count++;
			while (start + count < stop && wd.isWordPart(d.getChar(start + count))) {
				count++;
			}
		} catch (BadLocationException ex) {
		}
		return count;
	}

	public static int calcLiteralTokenLength(IDocument d, int start, int stop, String escapeChars) {
		int count = 0;
		int pair;
		try {
			pair = d.getChar(start);
		} catch (BadLocationException ex) {
			return 0;
		}
		if (pair != '"' && pair != '\'') return 0;
		count++;
		while ( start + count < stop) {
			try {
                int character = d.getChar(start + count);
                if (escapeChars != null && escapeChars.indexOf(character) != -1) count++; // Skip next char
				else if (character == pair) return (count + 1);
			} catch (BadLocationException ex) {
				return 0;
			}
			count++;
		}
		return 0;
	}
	
	public static String rtrim(String text) {
		try {
			IDocument d = new Document(text);
			int length = d.getLength();
			while (length > 0 && TextScanner.WS_DETECTOR.isWhitespace(d.getChar(length - 1))) {
				length--;
			}
			return d.get(0, length);
		} catch (Exception ex) {
			return text;
		}
	}
	
	static class NameTokenDetector implements IWordDetector {
		/**
		 * @see IWordDetector#isWordPart(char)
		 */
		public boolean isWordPart( char ch ) {
			if ( Character.isUnicodeIdentifierPart(ch) ) return true;

			switch ( ch ) {
				case '.' : case '-' : case '_' : case ':' :
					return true;
			};

			return false;
		}

		/**
		 * @see IWordDetector#isWordStart(char)
		 */
		public boolean isWordStart( char ch ) {
			return isWordPart( ch );
		}
	}
	
	static class WhitespaceDetector implements IWhitespaceDetector {
		/**
		 * @see IWhitespaceDetector#isWhitespace(char)
		 */
		public boolean isWhitespace( char ch ) {
			switch ( ch ) {
				case 0x09:
				case 0x0A:
				case 0x0D:
				case 0x20:
					return true;

				default :
					return false;
			}
		}
	}

}