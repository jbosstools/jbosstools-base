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
package org.jboss.tools.common.model.ui.views.palette;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Properties;
import java.util.StringTokenizer;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.eclipse.ui.texteditor.ITextEditor;

import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;
import org.jboss.tools.jst.web.tld.TLDToPaletteHelper;
import org.jboss.tools.jst.web.tld.URIConstants;

/**
 * @author Jeremy
 */
public class PaletteInsertHelper {
	
	public static final String PROPOPERTY_TAG_NAME   = "tag name";
	public static final String PROPOPERTY_START_TEXT = TLDToPaletteHelper.START_TEXT;
	public static final String PROPOPERTY_END_TEXT   = TLDToPaletteHelper.END_TEXT;
	public static final String PROPOPERTY_NEW_LINE = "new line";
	public static final String PROPOPERTY_REFORMAT_BODY  = TLDToPaletteHelper.REFORMAT;
	public static final String PROPOPERTY_TAGLIBRARY_URI = URIConstants.LIBRARY_URI;
	public static final String PROPOPERTY_TAGLIBRARY_VERSION = URIConstants.LIBRARY_VERSION;
	public static final String PROPOPERTY_DEFAULT_PREFIX = URIConstants.DEFAULT_PREFIX;
	public static final String PROPOPERTY_SELECTION_PROVIDER = "selectionProvider";
    public static final String PROPOPERTY_ADD_TAGLIB = TLDToPaletteHelper.ADD_TAGLIB;
    static PaletteTaglibInserter PaletteTaglibInserter = new PaletteTaglibInserter();

	public static void insertIntoEditor(ITextEditor editor, Properties p) {
		if(editor == null) return;
		if(!isEditable(editor)) {
			ServiceDialog d = PreferenceModelUtilities.getPreferenceModel().getService();
			String name = editor.getEditorInput().getName();
			String mes = "Source " + name + " is read-only";
			if(editor != null && isEditable(editor.getEditorInput())) {
				mes = "Please activate Source tab.";
			}
			d.showDialog("Warning", mes, new String[]{"OK"}, null, ServiceDialog.WARNING);
			return;
		}
        try {
   			IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
   			ISelectionProvider selProvider = editor.getSelectionProvider();
   			p.put(PROPOPERTY_SELECTION_PROVIDER, selProvider);
   			insertIntoEditorInternal(doc, p);
        } catch (Exception x) {
        	//ignore
        }
	}
	
	
	static boolean isEditable(ITextEditor editor) {
		if(editor == null) return false;
		return editor.isEditable();
	}
	static boolean isEditable(IEditorInput input) {
		if(input instanceof IFileEditorInput) {
			IFile f = ((IFileEditorInput)input).getFile();
			return f != null && !f.isReadOnly();
		} else if(input instanceof IModelObjectEditorInput) {
			XModelObject o = ((IModelObjectEditorInput)input).getXModelObject();
			if(o != null && !o.isObjectEditable()) return false;
		}
		return true;
	}

	public static void insertIntoEditor(ISourceViewer v, Properties p) {
		try {
			String tagname = p.getProperty(PROPOPERTY_TAG_NAME);
			String startText = p.getProperty(PROPOPERTY_START_TEXT);
			String endText = p.getProperty(PROPOPERTY_END_TEXT);
			String uri = p.getProperty(PROPOPERTY_TAGLIBRARY_URI);
						
			ISelectionProvider selProvider = (ISelectionProvider)p.get(PROPOPERTY_SELECTION_PROVIDER);
			if(selProvider == null) p.put(PROPOPERTY_SELECTION_PROVIDER, v.getSelectionProvider());


			IDocument d = v.getDocument();
			String[] texts = new String[]{startText, endText};
			p = PaletteTaglibInserter.inserTaglib(v, p);			
			String defaultPrefix = p.getProperty(PROPOPERTY_DEFAULT_PREFIX);
			applyPrefix(texts, d, tagname, uri, defaultPrefix);						
			startText = texts[0];
			endText = texts[1];
			
			if(startText != null) p.setProperty(PROPOPERTY_START_TEXT, startText);
			if(endText != null) p.setProperty(PROPOPERTY_END_TEXT, endText);

	        try {
	    		if (v != null ) {
	    			IEditorPart activeEditor = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
	    			IDocument doc = v.getDocument();
	    			insertIntoEditorInternal(doc, p);
	    			/*
	    			 * Leave as is
	    			 */
	    			if(v instanceof IIgnoreSelection) {
	    				((IIgnoreSelection)v).setIgnore(true);
	    			}
	    			if (activeEditor != null)
	    				activeEditor.getSite().getPage().activate(activeEditor);
	    			/*
	    			 * Set Ignore false, to prevent focus losing. 
	    			 */
	    			if(v instanceof IIgnoreSelection) {
	    				((IIgnoreSelection)v).setIgnore(false);
	    			}
	    		}
	        } catch (Exception x) {
				ModelUIPlugin.log("Error while inserting text into editor", x);
	        }
		} catch (Exception e) {
			ModelUIPlugin.log(e);
		}
	}

	private static void insertIntoEditorInternal (IDocument doc, Properties p) {
		String startText = p.getProperty(PROPOPERTY_START_TEXT);
		String endText = p.getProperty(PROPOPERTY_END_TEXT);
		String newline = p.getProperty(PROPOPERTY_NEW_LINE);
		boolean reformat = "yes".equals(p.getProperty(PROPOPERTY_REFORMAT_BODY));
		ISelectionProvider selProvider = (ISelectionProvider)p.get(PROPOPERTY_SELECTION_PROVIDER);

		if (doc == null || selProvider == null) return;
		
		ITextSelection selection = (ITextSelection)selProvider.getSelection();
		int offset = selection.getOffset();
		int length = selection.getLength(); 
		
		//Changed due to new WTP version 1.5 R 2006.06.28 get selected text from document.
		String body = null;
		try {
			body = length > 0 ? doc.get(offset, length): "";
		} catch (BadLocationException e1) {
			ModelUIPlugin.log(e1);
		} 

		if (startText == null) startText = "";
		else startText = prepare(prepare(startText, "\\n", getLineDelimiter(doc)), "\\t", "\t");
		if (endText == null) endText = "";
		else endText = prepare(prepare(endText, "\\n", getLineDelimiter(doc)), "\\t", "\t");

		String text = reformat ? format (doc, offset, length, body, startText, endText, newline) : (startText + body + endText);
		
		//Remove empty line before startText if text starts with creating new line 
		String lineDelimiter = getLineDelimiter(doc);
		if(reformat && text.startsWith(lineDelimiter)) {
			try {
				int ln = doc.getLineOfOffset(offset);
				int off = doc.getLineOffset(ln);
				if(off == offset) {
					text = text.substring(1);
				} else {
					String s = doc.get(off, offset - off).trim();
					if(s.length() == 0) {
						text = text.substring(lineDelimiter.length());
						length += offset - off;
						offset = off;
					}					
				}
			} catch (BadLocationException e) {
				ModelUIPlugin.log(e);
			}
		}
		
		int pos = text.indexOf("|");
		if (pos >= 0) {
			text = text.substring(0, pos) + text.substring(pos + 1);
		} else {
			pos = text.length();
		}

		try {
			doc.replace(offset, length, text);
		} catch (BadLocationException ex) {
			ModelUIPlugin.log(ex);
		}

		ITextSelection sel = new TextSelection(offset + pos, 0);
		selProvider.setSelection(sel);
	}
	
	private static String prepare (String text, String pattern, String replacer) {
		String res = text;
		int index;
		while ((index = res.indexOf(pattern)) != -1) {
			res = res.substring(0, index) + replacer + res.substring(index + pattern.length());
		}
		return res;
	}
    
    private static String format(IDocument d, int offset, int length, String body, String startText, String endText, String newline) {

    	String lineDelimiter = getLineDelimiter(d);

		boolean indentBody = (startText != null && startText.length() > 0 && 
								endText != null && endText.length() > 0);

		String firstLineIndent = "";
        boolean indentFirstLine = false;
        if (length == 0) {
            firstLineIndent = getIndentOfLineOfOffset(d, offset);
        } else {
            firstLineIndent = getIndentOfFirstLine(d, offset);
            indentFirstLine = true;
        }
		String lastLineIndent = "";

		boolean appendFirstDelimiter = true;
		try {
			if (d.getLineOffset(d.getLineOfOffset(offset)) == offset) 
				appendFirstDelimiter = false; // At start of a line
		} catch (Exception ex) {
			//ignore
		}

        if (body == null || body.length() == 0) appendFirstDelimiter = false;
        
		boolean appendLastDelimiter = true;
		try {
			int line = d.getLineOfOffset(offset + length);
			int lineOffset = d.getLineOffset(line);
			int lineLength = d.getLineInformation(line).getLength();

			lastLineIndent = getIndentOfLine(d.get(offset + length, lineOffset + lineLength - offset - length), lineDelimiter);
			
			if (lineOffset + lineLength - offset - length == 0) 
				appendLastDelimiter = false;
		} catch (Exception ex) {
			
		}

		final StringBuffer buffer= new StringBuffer();
		if (startText != null && startText.length() > 0) {
			if (appendFirstDelimiter) buffer.append(lineDelimiter);
            if (indentFirstLine) buffer.append(firstLineIndent);
            StringTokenizer st = new StringTokenizer(startText, "\n", true);
            boolean isFirst = true;
            while(st.hasMoreTokens()) {
            	String t = st.nextToken();
            	if("\n".equals(t)) {
            		buffer.append(t);
            		isFirst = false;
            	} else {
            		if(!isFirst) buffer.append(firstLineIndent);
            		buffer.append(t);
            	}
            }
            if (!"false".equals(newline))
            	buffer.append(lineDelimiter);            
            
			body = (body == null || body.length() == 0) ? "" : firstLineIndent + body.substring(getIndentOfLine(body, lineDelimiter).length());
		}
		int deltaSize = indentBody ? getTabWidth() : 0;
		boolean appendPreEndLineDelimiter = true;
        if (body != null && body.length() > 0) {
    		for (final Iterator iterator= new LineIterator(body); iterator.hasNext();) {
    			Object o = iterator.next();
    			String line= (o == null) ? null : o.toString();
    			
    			String lineIndent = getIndentOfLine(line, getLineDelimiter(d));
    			String lineContent= line.substring(lineIndent.length());
    			appendPreEndLineDelimiter = true;
    			if (lineContent.length() == 0) {
    				// line was empty; insert as is
    				buffer.append(line);
    				if (!iterator.hasNext())
    					appendPreEndLineDelimiter = false;
    			} else {
    				int indentSize= calculateDisplayedWidth(lineIndent, getTabWidth());
    				lineIndent= changePrefix(lineIndent.trim(), indentSize + deltaSize, useSpaces(), getTabWidth());
    				buffer.append(lineIndent);
    				buffer.append(lineContent);			
    			}
    			if (iterator.hasNext())
    				buffer.append(lineDelimiter);			
    		}
        } else {
                String lineIndent = changePrefix(firstLineIndent, 
                    calculateDisplayedWidth(firstLineIndent, getTabWidth()) + deltaSize, 
                    useSpaces(), getTabWidth());
                buffer.append(lineIndent);  
                
                if ((startText.indexOf('|') == -1)&&  
                	(endText.indexOf('|') == -1))
                		buffer.append('|');
        }
		if (endText != null && endText.length() > 0) {
			if (appendPreEndLineDelimiter){
				buffer.append(lineDelimiter);
            }
			buffer.append(firstLineIndent);
			buffer.append(endText);
			if (appendLastDelimiter) {
				buffer.append(lineDelimiter);
				int indentLength = calculateDisplayedWidth(firstLineIndent, getTabWidth()) -
					calculateDisplayedWidth(lastLineIndent, getTabWidth());
				if (indentLength > 0)
					buffer.append(changePrefix(lastLineIndent.trim(), indentLength, useSpaces(), getTabWidth()));
			}
		}

		return buffer.toString();
	}
	
    private static int getTabWidth() {
		try {
			return Platform.getPreferencesService().getInt("org.eclipse.ui.editors", AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH, 4, new IScopeContext[]{new InstanceScope()}); 
		} catch (Exception e) {
			ModelUIPlugin.log(e);
			return 4;
		}
	}

    private static boolean useSpaces() {
    	return false;
    }

	/**
	 * Returns the displayed width of a string, taking in account the displayed tab width.
	 * The result can be compared against the print margin.
	 */
	private static int calculateDisplayedWidth(String string, int tabWidth) {

		int column= 0;
		for (int i= 0; i < string.length(); i++)
			if ('\t' == string.charAt(i))
				column += tabWidth - (column % tabWidth);
			else
				column++;

		return column;
	}

	/**
	 * Extends the string to match displayed width.
	 * String is either the empty string or "//" and should not contain whites.
	 */
	private static String changePrefix(String string, int displayedWidth, boolean useSpaces, int tabWidth) {

		// assumption: string contains no whitspaces
		final StringBuffer buffer= new StringBuffer(string);
		int column= calculateDisplayedWidth(buffer.toString(), tabWidth);

		if (column > displayedWidth)
			return string;
		
		if (useSpaces) {
			while (column != displayedWidth) {
				buffer.append(' ');
				++column;
			}
			
		} else {
			
			while (column != displayedWidth) {
				if (column + tabWidth - (column % tabWidth) <= displayedWidth) {
					buffer.append('\t');
					column += tabWidth - (column % tabWidth);
				} else {
					buffer.append(' ');
					++column;
				}
			}			
		}

		return buffer.toString();
	}
	
	public static String getLineDelimiter(IDocument document) {
		try {
			if (document.getNumberOfLines() > 1)
				return document.getLineDelimiter(0);
		} catch (BadLocationException e) {
		}

		return System.getProperty("line.separator"); //$NON-NLS-1$
	}
	
	private static String getIndentOfFirstLine(IDocument d, int offset) {
		String indent = "";
		try {
			int line = d.getLineOfOffset(offset);
			while (line >= 0) {
				String lineText = d.get(d.getLineOffset(line), d.getLineLength(line));
				if (lineText.trim().length() > 0) {
					return getIndentOfLine(lineText, getLineDelimiter(d));
				}
				line--; 
			}
		} catch (Exception ex) {
		}
		return indent;
	}

    private static String getIndentOfLineOfOffset(IDocument d, int offset) {
        String indent = "";
        try {
            int line = d.getLineOfOffset(offset);
            String lineText = d.get(d.getLineOffset(line), d.getLineLength(line));
            return getIndentOfLine(lineText, getLineDelimiter(d));
        } catch (Exception ex) {
        }
        return indent;
    }

	private static String getIndentOfLine(String line, String lineDelimiter) {
		int i= 0;
		for (; i < line.length(); i++) {
            if (!Character.isWhitespace(line.charAt(i)))
                break;
            if (lineDelimiter != null && lineDelimiter.indexOf(line.charAt(i)) != -1)
                break;
		}
		return line.substring(0, i);
	}

	private static final class LineIterator implements Iterator {
		/** The document to iterator over. */
		private final IDocument fDocument;
		/** The line index. */
		private int fLineIndex;

		/**
		 * Creates a line iterator.
		 */
		public LineIterator(String string) {
			fDocument= new Document(string);
		}

		/**
		 * @see java.util.Iterator#hasNext()
		 */
		public boolean hasNext() {
			return fLineIndex != fDocument.getNumberOfLines();
		}

		/*
		 * @see java.util.Iterator#next()
		 */
		public Object next() {
			try {
				IRegion region= fDocument.getLineInformation(fLineIndex++);
				return fDocument.get(region.getOffset(), region.getLength());
			} catch (BadLocationException e) {
				ModelUIPlugin.log(e);
				throw new NoSuchElementException();
			}
		}

		/*
		 * @see java.util.Iterator#remove()
		 */
		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

	/**
	 * adding prefix to tag
	 */
	public static void applyPrefix(String[] text, ITextEditor editor, String tagname, String uri, String defaultPrefix) {
		if(defaultPrefix == null || defaultPrefix.length() == 0) return;
		IDocument doc = null;
        try {
   			doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
        } catch (Exception x) {
        	//ignore
        }
        applyPrefix(text, doc, tagname, uri, defaultPrefix);
	}

	public static void applyPrefix(String[] text, IDocument doc, String tagname, String uri, String defaultPrefix) {
        if(doc == null) return;
        String body = doc.get();
        applyPrefix(text, body, tagname, uri, defaultPrefix);
	}
	
	public static void applyPrefix(String[] text, String body, String tagname, String uri, String defaultPrefix) {
		if(uri == null || uri.length() == 0) return;
		Properties p = getPrefixes(body);
		String prefix = p.getProperty(uri, defaultPrefix);
		if(prefix == null || prefix.length() == 0) return;				
		for (int i = 0; i < text.length; i++) text[i] = applyPrefix(text[i], tagname, prefix, p);
	}

	static String applyPrefix(String text, String tagname, String prefix, Properties prefixes) {
		if(text == null || text.length() == 0) return text;
		if(tagname == null || tagname.length() == 0) return text;
		while(true) {
			int i = text.indexOf("%prefix|");
			if(i < 0) break;
			int j = text.indexOf("%", i + 8);
			if(j < 0) break;
			int j1 = text.indexOf("|", i + 8);
			String uri = "";
			String defaultPrefix = "";
			String pr = "";
			try {
				uri = text.substring(i + 8, j1);
				defaultPrefix = text.substring(j1 + 1, j);
				pr = prefixes.getProperty(uri, defaultPrefix);
			} catch (Exception e) {
				ModelUIPlugin.log(e);
			}
			if(pr.length() > 0) {
				text = text.substring(0, i) + pr + ":" + text.substring(j + 1);
			} else {
				text = text.substring(0, i) + text.substring(j + 1);
			}
		}

		int k = text.toLowerCase().indexOf(":" + tagname.toLowerCase());
		if(k >= 0) {
			int g = text.indexOf("</");
			if(g >= 0 && g < k) {
				return text.substring(0, g + 2) + prefix + text.substring(k);
			}
			g = text.indexOf("<");
			if(g >= 0 && g < k) {
				return text.substring(0, g + 1) + prefix + text.substring(k);
			}
		}
		k = text.toLowerCase().indexOf("<" + tagname.toLowerCase());
		if(k >= 0) {
			return text.substring(0, k + 1) + prefix + ":" + text.substring(k + 1);
		}
		k = text.toLowerCase().indexOf("</" + tagname.toLowerCase());
		if(k >= 0) {
			return text.substring(0, k + 2) + prefix + ":" + text.substring(k + 2);
		}
		return text;
	}

	static Properties getPrefixes(String body) {
		Properties p = new Properties();
		int i = 0;
		while(i >= 0 && i < body.length()) {
			i = body.indexOf("<%@ taglib ", i);
			if(i < 0) break;
			int j = body.indexOf("%>", i);
			if(j < 0) j = body.length();
			String taglib = body.substring(i, j);
			getPrefix(p, taglib);
			i = j + 1;
		}
		return p;
	}
	
	static void getPrefix(Properties p, String taglib) {
		int i = taglib.indexOf("uri=\"");
		if(i < 0) return;
		int j = taglib.indexOf("\"", i + 5);
		if(j < 0) return;
		String uri = taglib.substring(i + 5, j);
		i = taglib.indexOf("prefix=\"");
		if(i < 0) return;
		j = taglib.indexOf("\"", i + 8);
		if(j < 0) return;
		String prefix = taglib.substring(i + 8, j);
		p.setProperty(uri, prefix);
	}

}