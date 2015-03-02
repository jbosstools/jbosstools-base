/*******************************************************************************
 * Copyright (c) 2007-2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.views.palette;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
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
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.wst.sse.core.StructuredModelManager;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.eclipse.wst.sse.core.internal.provisional.text.IStructuredDocument;
import org.eclipse.wst.xml.core.internal.document.CommentImpl;
import org.eclipse.wst.xml.core.internal.document.ElementImpl;
import org.eclipse.wst.xml.core.internal.document.DocumentTypeImpl;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMDocument;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMModel;
import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.options.SharableConstants;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;
import org.jboss.tools.common.model.ui.editors.dnd.IElementGenerator;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

/**
 * @author Victor Rubezhny
 */
public class PaletteInsertHelper {

	public static final String PROPERTY_TAG_NAME   = "tag name"; //$NON-NLS-1$
	public static final String PROPERTY_START_TEXT = XModelObjectConstants.START_TEXT;
	public static final String PROPERTY_END_TEXT   = XModelObjectConstants.END_TEXT;
	public static final String PROPERTY_NEW_LINE = "new line"; //$NON-NLS-1$
	public static final String PROPERTY_REFORMAT_BODY  = XModelObjectConstants.REFORMAT;
	public static final String PROPERTY_SELECTION_PROVIDER = "selectionProvider"; //$NON-NLS-1$

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
		p.put(PROPERTY_SELECTION_PROVIDER, selProvider);
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
		String startText = p.getProperty(PROPERTY_START_TEXT);
		String endText = p.getProperty(PROPERTY_END_TEXT);
	
		ISelectionProvider selProvider 
				= (ISelectionProvider)p.get(PROPERTY_SELECTION_PROVIDER);
		if(selProvider == null) {
			p.put(PROPERTY_SELECTION_PROVIDER, v.getSelectionProvider());
		}
	
		IDocument d = v.getDocument();
		
		String[] texts = new String[] {startText, endText};

		//do any auxiliary job here
		modify(v, p, texts);
	
		startText = texts[0];
		endText = texts[1];
	
		if(startText != null) {
			p.setProperty(PROPERTY_START_TEXT, startText);
		}
		if(endText != null) {
			p.setProperty(PROPERTY_END_TEXT, endText);
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

	public int correctOffset(IDocument document, int offset){
		return correctOffset(document, offset, null);
	}

	public int correctOffset(IDocument document, int offset, String paletteItemPath){
		ITextSelection selection = correctSelection(document, new TextSelection(document, offset, 0), paletteItemPath);
		return selection.getOffset();
	}

	public ITextSelection correctSelection(IDocument document, ITextSelection selection){
		return correctSelection(document, selection, null);
	}

	public ITextSelection correctSelection(IDocument document, ITextSelection selection, String paletteItemPath){
		int start = selection.getOffset();
		int end = start + selection.getLength();
		IStructuredModel model = null;
		try{
			model = StructuredModelManager.getModelManager().getExistingModelForRead((IStructuredDocument)document);
			IDOMDocument xmlDocument = (model instanceof IDOMModel) ? ((IDOMModel) model).getDocument() : null;
			if(xmlDocument != null){
				if(paletteItemPath != null) {
					 IPositionCorrector corrector = PaletteInsertManager.getInstance().createCorrectorInstance(paletteItemPath);
					 if(corrector != null) {
						 selection = corrector.correctSelection(xmlDocument, selection);
						 start = selection.getOffset();
						 end = start + selection.getLength();
					 }
				}
				if(start == end){
					IndexedRegion region = model.getIndexedRegion(start);
					if (region instanceof ElementImpl) {
						ElementImpl element = (ElementImpl) region;
						int startOffset = element.getStartOffset();
						int startEndOffset = element.getStartEndOffset();
						int endStartOffset = element.getEndStartOffset();
						int endOffset = element.getEndOffset();
						if(start >= startOffset && start <= startEndOffset){
							if(start-startOffset < startEndOffset-start){
								start = end = startOffset;
							}else{
								start = end = startEndOffset;
							}
						}else if(start >= endStartOffset && start <= endOffset){
							if(start-endStartOffset < endOffset-start){
								start = end =  endStartOffset;
							}else{
								start = end =  endOffset;
							}
						}
					}else if(region instanceof DocumentTypeImpl){
						DocumentTypeImpl element = (DocumentTypeImpl) region;
						int startOffset = element.getStartOffset();
						int endOffset = element.getEndOffset();
						if(start >= startOffset && start <= endOffset){
							if(start-startOffset < endOffset-start){
								start = end = startOffset;
							}else{
								start = end = endOffset;
							}
						}else if(start >= endOffset && start <= endOffset){
							if(start-endOffset < endOffset-start){
								start = end =  endOffset;
							}else{
								start = end =  endOffset;
							}
						}
					} else if(region instanceof CommentImpl) {
						int startOffset = region.getStartOffset();
						int endOffset = region.getEndOffset();
						if(start >= startOffset && start <= endOffset) {
							if(start-startOffset < endOffset-start){
								start = end = startOffset;
							}else{
								start = end = endOffset;
							}
						}
					}
				}else{
					IndexedRegion startRegion = model.getIndexedRegion(start);
					IndexedRegion endRegion = model.getIndexedRegion(end);
					if(startRegion != null && endRegion != null){
						IndexedRegion commonRegion = findCommonRegion(startRegion, endRegion, start, end);
						if(commonRegion != null){
							if(commonRegion instanceof ElementImpl){
								ElementImpl root = (ElementImpl) commonRegion;
								ElementImpl firstElement = findElement(root, start, end);
								if(firstElement != null){
									ElementImpl lastElement = firstElement;
									Node lastNode = firstElement; 
									while(lastNode.getNextSibling() != null){
										lastNode = lastNode.getNextSibling(); 
										if(lastNode instanceof ElementImpl){
											ElementImpl element = (ElementImpl)lastNode;
											if((element.getStartEndOffset() != element.getEndOffset() && element.getStartEndOffset() > start && element.getEndStartOffset() < end) ||
													(element.getStartEndOffset() == element.getEndOffset() && element.getStartOffset() >= start && element.getEndOffset() <= end)){
												lastElement = (ElementImpl)lastNode; 
											}else{
												break;
											}
										}
									}
									
									if(firstElement != null){
										start = firstElement.getStartOffset();
										if(lastElement != null){
											end = lastElement.getEndOffset();
										}else{
											end = firstElement.getEndOffset();
										}
									}
								}else{
									// first element -null
									start = end = findMinimalDistance(startRegion, endRegion, start, end);
								}
							}
						}else{
							// common region -null
							start = end = findMinimalDistance(startRegion, endRegion, start, end);
						}
					}
				}
			}
		}finally{
			if (model != null) model.releaseFromRead();
		}
		
		return new TextSelection(document, start, end-start);
	}
	
	private int findMinimalDistance(IndexedRegion startRegion, IndexedRegion endRegion, int start, int end){
		int middle = start + (end-start)/2;
		ElementImpl startElement = null;
		if(startRegion instanceof Text){
			startElement = (ElementImpl)((Text)startRegion).getParentNode();
		}else if(startRegion instanceof ElementImpl){
			startElement = (ElementImpl)startRegion;
		}
		ElementImpl endElement = null;
		if(endRegion instanceof Text){
			endElement = (ElementImpl)((Text)endRegion).getParentNode();
		}else if(endRegion instanceof ElementImpl){
			endElement = (ElementImpl)endRegion;
		}
		
		int[] offsets=new int[]{
				startElement.getStartOffset(),
				startElement.getStartEndOffset(),
				startElement.getEndStartOffset(),
				startElement.getEndOffset(),
				endElement.getStartOffset(),
				endElement.getStartEndOffset(),
				endElement.getEndStartOffset(),
				endElement.getEndOffset()
			};
		
		int[] distances=new int[8];
		for(int i = 0; i < 8; i++){
			distances[i] = Math.abs(offsets[i]-middle);
		}
		
		int minDistance = distances[0];
		int offset = offsets[0];
		for(int index = 0; index < distances.length; index++){
			if(distances[index] < minDistance){
				minDistance = distances[index];
				offset = offsets[index];
			}
		}
		
		return offset;
	}
	
	private ElementImpl findElement(ElementImpl root, int start, int end){
		if((root.getStartEndOffset() != root.getEndOffset() && root.getStartEndOffset() > start && root.getEndStartOffset() < end) ||
				(root.getStartEndOffset() == root.getEndOffset() && root.getStartOffset() >= start && root.getEndOffset() <= end)){
			return root;
		}else{
			NodeList list = root.getChildNodes();
			for(int index = 0; index < list.getLength(); index++){
				Node child = list.item(index);
				if(child instanceof ElementImpl){
					ElementImpl result = findElement((ElementImpl)child, start, end);
					if(result != null){
						return result;
					}
				}
			}
		}
		return null;
	}
	
	
	private IndexedRegion findCommonRegion(IndexedRegion startRegion, IndexedRegion endRegion, int start, int end){
		if(startRegion.getStartOffset() == endRegion.getStartOffset()){
			return startRegion;
		}
		
		IndexedRegion startElement = null;
		if(startRegion instanceof Text){
			startElement = (IndexedRegion)((Text)startRegion).getParentNode();
		}else if(startRegion instanceof ElementImpl){
			startElement = startRegion;
		}
		IndexedRegion endElement = null;
		if(endRegion instanceof Text){
			endElement = (IndexedRegion)((Text)endRegion).getParentNode();
		}else if(endRegion instanceof ElementImpl){
			endElement = endRegion;
		}
		
		// startElement loop
		while(startElement != null){
			
			// endElement loop
			IndexedRegion elem = endElement;
			while(elem != null){
				if(startElement.getStartOffset() == elem.getStartOffset()){
					return (IndexedRegion)startElement;
				}
				elem = (IndexedRegion)((Node)elem).getParentNode();
			}
			startElement = (IndexedRegion)((Node)startElement).getParentNode();
		}
		
		return null;
	}

	protected void insertIntoEditorInternal(IDocument doc, Properties p) {
		String startText = p.getProperty(PROPERTY_START_TEXT);
		String endText = p.getProperty(PROPERTY_END_TEXT);
		String newline = p.getProperty(PROPERTY_NEW_LINE);
		boolean reformat = "yes".equals(p.getProperty(PROPERTY_REFORMAT_BODY)); //$NON-NLS-1$
		ISelectionProvider selProvider = (ISelectionProvider)p.get(PROPERTY_SELECTION_PROVIDER);

		if (doc == null || selProvider == null) return;

		ITextSelection selection = (ITextSelection)selProvider.getSelection();
		
		selection = correctSelection(doc, selection, p.getProperty(SharableConstants.PALETTE_PATH));
		
		int offset = selection.getOffset();
		int length = selection.getLength();

		if (startText == null) startText = ""; //$NON-NLS-1$
		else startText = prepare(prepare(startText, "\\n", getLineDelimiter(doc)), "\\t", "\t"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		if (endText == null) endText = ""; //$NON-NLS-1$
		else endText = prepare(prepare(endText, "\\n", getLineDelimiter(doc)), "\\t", "\t"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		int start = offset;
		int end = offset + length;
		
		String body = "";
		try {
			int firstLine = doc.getLineOfOffset(start);
			int firstLineStart = doc.getLineOffset(doc.getLineOfOffset(start));
			
			String firstLineBeginning = start == firstLineStart ? 
					"" : doc.get(firstLineStart, start - firstLineStart).trim();
			if (firstLineBeginning.isEmpty()) {
				start = firstLine == 0 ?
						doc.getLineOffset(firstLine) :
						doc.getLineOffset(firstLine - 1) + doc.getLineLength(firstLine - 1) -
						getLineDelimiter(doc, firstLine - 1).length();
			}
			
			int lastLine = doc.getLineOfOffset(end);
			int lastLineEnd = doc.getLineOffset(lastLine) + doc.getLineLength(lastLine) - 
					getLineDelimiter(doc, lastLine).length();
			String lastLineEnding = end == lastLineEnd ? "" : doc.get(end, lastLineEnd - end).trim();
			if (lastLineEnding.isEmpty()) {
				end = lastLineEnd;
			}
			
			//Changed due to new WTP version 1.5 R 2006.06.28 get selected text from document.
			body =  end == start? "" : doc.get(start, end - start); //$NON-NLS-1$
		} catch (BadLocationException e1) {
			ModelUIPlugin.getPluginLog().logError(e1);
		}
		
		String text = reformat ? formatText (doc, start, end - start, body, selection, startText, endText, newline) : (startText + body + endText);

		int pos = text.indexOf("|"); //$NON-NLS-1$
		if (pos >= 0) {
			text = text.substring(0, pos) + text.substring(pos + 1);
		} else {
			pos = text.length();
		}

		if(start >=0 && end <= doc.getLength() && (end - start) >= 0 && !text.isEmpty()){
			try {
				doc.replace(start, end - start, text);
			} catch (BadLocationException ex) {
				ModelUIPlugin.getPluginLog().logError(ex);
			}
			ITextSelection sel = new TextSelection(start + pos, 0);
			selProvider.setSelection(sel);
		}
	}

	protected static String prepare (String text, String pattern, String replacer) {
		String res = text;
		int index;
		while ((index = res.indexOf(pattern)) != -1) {
			res = res.substring(0, index) + replacer + res.substring(index + pattern.length());
		}
		return res;
	}
	
	protected static String createIndent(String line, boolean increase, String lineDelimiter) {
		String indentString = getIndentOfLine(line, lineDelimiter);
		int tabWidth = IElementGenerator.NodeWriter.getTabWidth();
		int displayedWidth = calculateDisplayedWidth(indentString, tabWidth);
		if (increase)
			displayedWidth += tabWidth;
		
		return createIndent(displayedWidth);
	}
	
	protected static String createIndent(int displayedWidth) {
		StringBuilder indent = new StringBuilder();
		int tabWidth = IElementGenerator.NodeWriter.getTabWidth();
		boolean useSpaces = IElementGenerator.NodeWriter.useSpaces();

		if (useSpaces) {
			while (indent.length() < displayedWidth) {
				indent = indent.append(' ');
			}
		} else {
			int width = 0;
			while (width < displayedWidth) {
				indent = indent.append(width + tabWidth <= displayedWidth ? '\t' : ' ');
				width += width + tabWidth <= displayedWidth ? tabWidth : 1;
			}
		}
		return indent.toString();
	}
	
	protected static String createIndent(IDocument doc, boolean increase, int offset) {
		String lineDelimiter = getLineDelimiter(doc);
		String lineText = "";
		try {
			int line = doc.getLineOfOffset(offset);
			while (line >= 0) {
				int lineOffset = doc.getLineOffset(line);
				int lineLength = doc.getLineLength(line);
				String text = lineLength == 0 ? "" : doc.get(lineOffset, lineLength);
				if (text.trim().length() > 0) {
					lineText = text;
					break;
				}
				line--;
			}
		} catch (BadLocationException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} 
		return createIndent(lineText, increase, lineDelimiter);
	}
	
	protected static boolean shouldIncreaseIndent(StringBuilder buffer, int offset) {
		return shouldIncreaseIndent(new Document(buffer.toString()), offset);
	}
	
	protected static boolean shouldIncreaseIndent(IDocument doc, int offset) {
		try {
			List<String> closingTags = new ArrayList<String>();
			
			// Find first non-empty line to calculate indents
			int line = doc.getLineOfOffset(offset);
			String text = "";
			while (line >= 0) {
				int lineOffset = doc.getLineOffset(line);
				int lineLength = doc.getLineLength(line);
				lineLength = offset < lineOffset + lineLength ? offset - lineOffset : lineLength;
				if (lineLength > 0) {
					String lineText = doc.get(lineOffset, lineLength);
					if (!lineText.trim().isEmpty()) {
						text = lineText;
						break;
					}
				}
				line--;
			}
			
			int index = text.length();
			while (index >= 0) {
				index = text.lastIndexOf('<', index - 1);
				if (index == -1) return false;

				boolean closingTag = (index + 1 >= text.length() ||  '/' == text.charAt(index + 1));
				int tagEnd = text.indexOf('>', index);
				boolean selfClosingTag = (tagEnd == -1 || '/' == text.charAt(tagEnd - 1) || ('-' == text.charAt(tagEnd - 1) && '-' == text.charAt(tagEnd - 2)));

				StringBuilder sb = new StringBuilder();
				for (int i = index + 1; i < tagEnd; i++) {
					char ch = text.charAt(i);
					if (ch == '/') continue;
					if (!Character.isJavaIdentifierStart(ch))
						break;
					sb.append(ch);
				}
				String tagName = sb.toString();
				if ("br".equalsIgnoreCase(tagName) || "hr".equals(tagName))
					continue;
				
				if (closingTag) {
					closingTags.add(tagName);
				}
				if (!closingTag && !selfClosingTag) {
					int closingTagIndex = closingTags.indexOf(tagName);
					if (closingTagIndex == -1)
						return true; // No according closing tag found - so, the tag isn't closed
					else
						closingTags.remove(closingTagIndex);
				}
			}				
		} catch (BadLocationException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		return false;
	}
	
	protected static int calMinIndentWidth(String text, String lineDelimiter) {
		if (text == null)
			return 0;
		
    	LineIterator textLines = new LineIterator(text);
    	int minIndentWidth = -1;
    	while (textLines.hasNext()) {
    		String line = (String)textLines.next();
    		int lineIndent = calculateDisplayedWidth(getIndentOfLine(line, lineDelimiter), IElementGenerator.NodeWriter.getTabWidth());
    		if (minIndentWidth == -1 || minIndentWidth > lineIndent) 
    			minIndentWidth = lineIndent;
    	}
    	return minIndentWidth;
	}
	
	protected static String formatText(IDocument d, int offset, int length, String body, ITextSelection selection, String startText, String endText, String newline) {
		String lineDelimiter = getLineDelimiter(d);
    	
    	startText = trimNewLines(startText);
    	endText = trimNewLines(endText);
    	body = body == null ? "" : body;
    	
 
    	int minStartTextIndentWidth = calMinIndentWidth(startText, lineDelimiter);
    	int minEndTextIndentWidth = calMinIndentWidth(endText, lineDelimiter);
    	int minBodyIndentWidth = calMinIndentWidth(body, lineDelimiter);
    	int tabWidth = IElementGenerator.NodeWriter.getTabWidth();
    	
		boolean indentBody = (startText.trim().length() > 0 && endText.trim().length() > 0);
		boolean increaseIndent = shouldIncreaseIndent(d, offset);
		String firstLineIndent = createIndent(d, increaseIndent, offset);
		int bodyIndentWidth = calculateDisplayedWidth(firstLineIndent, tabWidth) + 
				(indentBody ? tabWidth : 0);
		int selectedLineIndentWidth = calculateDisplayedWidth(
				createIndent(d, false, selection.getOffset()), tabWidth);
		
		String bodyPrefix = "";
		String bodySuffix = "";
		try {
			bodyPrefix = selection.getOffset() == offset ? "" : d.get(offset, selection.getOffset() - offset);
			bodySuffix = offset + length == selection.getOffset() + selection.getLength() ?
    				"" : d.get(selection.getOffset() + selection.getLength(), 
    						offset + length - selection.getOffset() - selection.getLength());
		} catch (BadLocationException e) {
    		ModelUIPlugin.getPluginLog().logError(e);
		}
    	
    	StringBuilder buffer = new StringBuilder();
    	
    	boolean notFirstOrLast = false;
    	try {
	    	int line = d.getLineOfOffset(offset);
	    	int lineOffset = d.getLineOffset(line);
	    	int lineLength = d.getLineLength(line);
	    	notFirstOrLast = !d.get(lineOffset, offset-lineOffset).trim().isEmpty() && !d.get(offset, lineOffset+lineLength-offset).trim().isEmpty();
    	} catch (BadLocationException ex) {
    		ModelUIPlugin.getPluginLog().logError(ex);
    	}
    	
    	boolean inlineFormatting = 
    			startText.indexOf('\n') == -1 && 
    			endText.indexOf('\n') == -1 &&
    			((selection.getLength() != 0 && selection.getText().indexOf('\n') == -1) || notFirstOrLast);
    	
    	if (inlineFormatting) {
	    	String insertLine = bodyPrefix + bodySuffix;
	  		
    		String prefix = "";
    		String indent = "";
	    	if (bodyPrefix.lastIndexOf('\n') != -1) {
		    	if (insertLine.trim().isEmpty()) {
		    		prefix = bodyPrefix.substring(0, bodyPrefix.lastIndexOf('\n') + 1);
		    	} else {
	    			prefix = bodyPrefix.substring(bodyPrefix.lastIndexOf('\n') + 1);
		    	}
		    	indent = firstLineIndent;
	    	}
	    	
	    	buffer.append(prefix)
	    		.append(createIndent(calculateDisplayedWidth(indent, tabWidth)))
    			.append(startText.trim())
    			.append(selection.getText())
    			.append(endText.trim());
    	} else {
	    	boolean appendLastDelimiter = true;
	    	try {
		    	int line = d.getLineOfOffset(offset + length);
		    	int lineOffset = d.getLineOffset(line);
		    	int lineLength = d.getLineLength(line) - (offset + length - lineOffset);
		    	String lastLine = lineLength == 0 ? "" : d.get(offset + length, lineLength);
		    	appendLastDelimiter = !lastLine.trim().isEmpty();
	    	} catch (BadLocationException ex) {
	    		ModelUIPlugin.getPluginLog().logError(ex);
	    	}
	    	
	    	int firstLineIndentWidth = calculateDisplayedWidth(firstLineIndent, tabWidth);
	
			int indentWidth = 0;
			if (!startText.trim().isEmpty()) {
				LineIterator textLines = new LineIterator(startText);
		    	while (textLines.hasNext()) {
		    		String line = (String)textLines.next();
			    	indentWidth = firstLineIndentWidth + calculateDisplayedWidth(getIndentOfLine(line, lineDelimiter), tabWidth) - minStartTextIndentWidth;
	
			    	buffer = buffer.append(lineDelimiter);
			    	buffer = buffer.append(createIndent(indentWidth));
			    	buffer = buffer.append(line.trim());
		    	}
			}
			
			if (!selection.getText().trim().isEmpty()) {
				
				bodyIndentWidth = indentWidth + (shouldIncreaseIndent(buffer, buffer.length()) ?
						tabWidth : 0);
		    	LineIterator textLines = new LineIterator(selection.getText());
		    	while (textLines.hasNext()) {
		    		String line = (String)textLines.next();
	
		    		indentWidth = bodyIndentWidth + calculateDisplayedWidth(getIndentOfLine(line, lineDelimiter), tabWidth) - minBodyIndentWidth;

			    	buffer = buffer.append(lineDelimiter);
			    	buffer = buffer.append(createIndent(indentWidth));
			    	buffer = buffer.append(line.trim());
		    	}
	        } else {
	
	            if ((startText.indexOf('|') == -1) &&  
	            	(endText.indexOf('|') == -1))
	            		buffer.append('|');        	
	        }
			
			if (!endText.trim().isEmpty()) {
				LineIterator textLines = new LineIterator(endText);
		    	while (textLines.hasNext()) {
		    		String line = (String)textLines.next();

			    	indentWidth = firstLineIndentWidth + calculateDisplayedWidth(getIndentOfLine(line, lineDelimiter), tabWidth) - minEndTextIndentWidth;
			    	indentWidth = firstLineIndentWidth + calculateDisplayedWidth(getIndentOfLine(line, lineDelimiter), tabWidth) - minStartTextIndentWidth;
				    	
			    	buffer = buffer.append(lineDelimiter);
			    	buffer = buffer.append(createIndent(indentWidth));
			    	buffer = buffer.append(line.trim());
		    	}		
			}
			
            if (appendLastDelimiter && !inlineFormatting) {
            	String ending = "";
            	int selectionEnd = selection.getOffset() + selection.getLength();
            	try {
            		int endLine = d.getLineOfOffset(selectionEnd);
                	int selectionLineEnd = d.getLineOffset(endLine) + d.getLineLength(endLine);
                	ending = selectionEnd == selectionLineEnd ?
                			"" : d.get(selectionEnd, selectionLineEnd - selectionEnd).trim();
				} catch (BadLocationException e) {
					ModelUIPlugin.getPluginLog().logError(e);
				}
            	
            	indentWidth = selectedLineIndentWidth;
            	
		    	buffer = buffer.append(lineDelimiter);
		    	buffer = buffer.append(createIndent(indentWidth));
            }
    	}    	
		return buffer.toString();
	}

	protected static String trimNewLines(String text) {
    	if (text == null) return "";
    	
    	char[] data = text.toCharArray();
    	int start = 0;
    	while (start < data.length) {
    		if ('\n' != data[start] && '\r' != data[start])
    			break;
    		start++;
    	}
    	
    	int end = data.length;
    	while (end > 0) {
    		if ('\n' != data[end - 1] && '\r' != data[end - 1])
    			break;
    		end--;
    	}
    	return new String(Arrays.copyOfRange(data, start, end));
    }

	/**
	 * Returns the displayed width of a string, taking in account the displayed tab width.
	 * The result can be compared against the print margin.
	 */
	protected static int calculateDisplayedWidth(String string, int tabWidth) {

		int column= 0;
		for (int i= 0; i < string.length(); i++)
			if ('\t' == string.charAt(i))
				column += tabWidth - (column % tabWidth);
			else
				column++;

		return column;
	}

	protected static String getLineDelimiter(IDocument document, int line) throws BadLocationException {
		String delim = document.getLineDelimiter(line);
		return delim == null ? "" : delim;
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

	protected static String getIndentOfLine(String line, String lineDelimiter) {
		int i= 0;
		for (; i < line.length(); i++) {
            if (!Character.isWhitespace(line.charAt(i)))
                break;
            if (lineDelimiter != null && lineDelimiter.indexOf(line.charAt(i)) != -1)
                break;
		}
		return line.substring(0, i);
	}

	protected static final class LineIterator implements Iterator {
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