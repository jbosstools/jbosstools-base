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

	public static final String PROPOPERTY_TAG_NAME   = "tag name"; //$NON-NLS-1$
	public static final String PROPOPERTY_START_TEXT = TLDToPaletteHelper.START_TEXT;
	public static final String PROPOPERTY_END_TEXT   = TLDToPaletteHelper.END_TEXT;
	public static final String PROPOPERTY_NEW_LINE = "new line"; //$NON-NLS-1$
	public static final String PROPOPERTY_REFORMAT_BODY  = TLDToPaletteHelper.REFORMAT;
	public static final String PROPOPERTY_TAGLIBRARY_URI = URIConstants.LIBRARY_URI;
	public static final String PROPOPERTY_TAGLIBRARY_VERSION = URIConstants.LIBRARY_VERSION;
	public static final String PROPOPERTY_DEFAULT_PREFIX = URIConstants.DEFAULT_PREFIX;
	public static final String PROPOPERTY_SELECTION_PROVIDER = "selectionProvider"; //$NON-NLS-1$
    public static final String PROPOPERTY_ADD_TAGLIB = TLDToPaletteHelper.ADD_TAGLIB;

    static PaletteInsertHelper instance = new PaletteInsertHelper();

    public static PaletteInsertHelper getInstance() {
    	return instance;
    }    
    
    public PaletteInsertHelper() {}

	public void insertIntoEditor(ITextEditor editor, Properties p) {
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
		IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
		ISelectionProvider selProvider = editor.getSelectionProvider();
		p.put(PROPOPERTY_SELECTION_PROVIDER, selProvider);
		insertIntoEditorInternal(doc, p);
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

	public void insertIntoEditor(final ISourceViewer v, Properties p) {
		String startText = p.getProperty(PROPOPERTY_START_TEXT);
		String endText = p.getProperty(PROPOPERTY_END_TEXT);
	
		ISelectionProvider selProvider 
				= (ISelectionProvider)p.get(PROPOPERTY_SELECTION_PROVIDER);
		if(selProvider == null) {
			p.put(PROPOPERTY_SELECTION_PROVIDER, v.getSelectionProvider());
		}
	
		IDocument d = v.getDocument();
		
		String[] texts = new String[] {startText, endText};

		//do any auxiliary job here
		modify(v, p, texts);
	
		startText = texts[0];
		endText = texts[1];
	
		if(startText != null) {
			p.setProperty(PROPOPERTY_START_TEXT, startText);
		}
		if(endText != null) {
			p.setProperty(PROPOPERTY_END_TEXT, endText);
		}

		IEditorPart activeEditor = ModelUIPlugin.getDefault().getWorkbench()
				.getActiveWorkbenchWindow().getActivePage().getActiveEditor();

		insertIntoEditorInternal(d, p);

		// Leave as is
		if(v instanceof IIgnoreSelection) {
			((IIgnoreSelection)v).setIgnore(true);
		}

		if (activeEditor != null) {
			activeEditor.getSite().getPage().activate(activeEditor);
		}

		// Set Ignore false, to prevent focus losing. 
		if(v instanceof IIgnoreSelection) {
			((IIgnoreSelection)v).setIgnore(false);
		}
	}

	protected void modify(ISourceViewer v, Properties p, String[] texts) {
		//override
	}

	protected void insertIntoEditorInternal(IDocument doc, Properties p) {
		String startText = p.getProperty(PROPOPERTY_START_TEXT);
		String endText = p.getProperty(PROPOPERTY_END_TEXT);
		String newline = p.getProperty(PROPOPERTY_NEW_LINE);
		boolean reformat = "yes".equals(p.getProperty(PROPOPERTY_REFORMAT_BODY)); //$NON-NLS-1$
		ISelectionProvider selProvider = (ISelectionProvider)p.get(PROPOPERTY_SELECTION_PROVIDER);

		if (doc == null || selProvider == null) return;

		ITextSelection selection = (ITextSelection)selProvider.getSelection();
		int offset = selection.getOffset();
		int length = selection.getLength(); 

		//Changed due to new WTP version 1.5 R 2006.06.28 get selected text from document.
		String body = null;
		try {
			body = length > 0 ? doc.get(offset, length): ""; //$NON-NLS-1$
		} catch (BadLocationException e1) {
			ModelUIPlugin.getPluginLog().logError(e1);
		}

		if (startText == null) startText = ""; //$NON-NLS-1$
		else startText = prepare(prepare(startText, "\\n", getLineDelimiter(doc)), "\\t", "\t"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		if (endText == null) endText = ""; //$NON-NLS-1$
		else endText = prepare(prepare(endText, "\\n", getLineDelimiter(doc)), "\\t", "\t"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

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
				ModelUIPlugin.getPluginLog().logError(e);
			}
		}

		int pos = text.indexOf("|"); //$NON-NLS-1$
		if (pos >= 0) {
			text = text.substring(0, pos) + text.substring(pos + 1);
		} else {
			pos = text.length();
		}

		try {
			doc.replace(offset, length, text);
		} catch (BadLocationException ex) {
			ModelUIPlugin.getPluginLog().logError(ex);
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

		String firstLineIndent = ""; //$NON-NLS-1$
        boolean indentFirstLine = false;
        if (length == 0) {
            firstLineIndent = getIndentOfLineOfOffset(d, offset);
        } else {
            firstLineIndent = getIndentOfFirstLine(d, offset);
            indentFirstLine = true;
        }
		String lastLineIndent = ""; //$NON-NLS-1$

		boolean appendFirstDelimiter = true;
		try {
			if (d != null && d.getLength() > offset && offset >= 0 
				&& d.getLineOffset(d.getLineOfOffset(offset)) == offset) 
				appendFirstDelimiter = false; // At start of a line
		} catch (BadLocationException ex) {
			ModelUIPlugin.getPluginLog().logError(ex);
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
		} catch (BadLocationException ex) {
			ModelUIPlugin.getPluginLog().logError(ex);
		}

		final StringBuffer buffer= new StringBuffer();
		if (startText != null && startText.length() > 0) {
			if (appendFirstDelimiter) buffer.append(lineDelimiter);
            if (indentFirstLine) buffer.append(firstLineIndent);
            StringTokenizer st = new StringTokenizer(startText, "\n", true); //$NON-NLS-1$
            boolean isFirst = true;
            while(st.hasMoreTokens()) {
            	String t = st.nextToken();
            	if("\n".equals(t)) { //$NON-NLS-1$
            		buffer.append(t);
            		isFirst = false;
            	} else {
            		if(!isFirst) buffer.append(firstLineIndent);
            		buffer.append(t);
            	}
            }
            if (!"false".equals(newline)) //$NON-NLS-1$
            	buffer.append(lineDelimiter);            

			body = (body == null || body.length() == 0) ? "" : firstLineIndent + body.substring(getIndentOfLine(body, lineDelimiter).length()); //$NON-NLS-1$
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
		return Platform.getPreferencesService().getInt("org.eclipse.ui.editors", AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH, 4, new IScopeContext[]{new InstanceScope()});  //$NON-NLS-1$
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
			ModelUIPlugin.getPluginLog().logError(e);
		}

		return System.getProperty("line.separator"); //$NON-NLS-1$
	}

	private static String getIndentOfFirstLine(IDocument d, int offset) {
		String indent = ""; //$NON-NLS-1$
		if(d == null) return indent;
		try {
			int line = d.getLineOfOffset(offset);
			while (line >= 0) {
				String lineText = d.get(d.getLineOffset(line), d.getLineLength(line));
				if (lineText.trim().length() > 0) {
					return getIndentOfLine(lineText, getLineDelimiter(d));
				}
				line--; 
			}
		} catch (BadLocationException ex) {
			ModelUIPlugin.getPluginLog().logError(ex);
		}
		return indent;
	}

    private static String getIndentOfLineOfOffset(IDocument d, int offset) {
        String indent = ""; //$NON-NLS-1$
        if(d == null) return indent;
        try {
            int line = d.getLineOfOffset(offset);
            String lineText = d.get(d.getLineOffset(line), d.getLineLength(line));
            return getIndentOfLine(lineText, getLineDelimiter(d));
        } catch (BadLocationException ex) {
			ModelUIPlugin.getPluginLog().logError(ex);
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
				ModelUIPlugin.getPluginLog().logError(e);
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

}