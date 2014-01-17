/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.model.ui.views.palette.test;

import java.util.Properties;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.eclipse.ui.texteditor.ITextEditor;
import org.jboss.tools.common.model.ui.views.palette.PaletteInsertHelper;
import org.jboss.tools.test.util.TestProjectProvider;
import org.jboss.tools.test.util.WorkbenchUtils;

/**
 * The JUnit test cases for issue https://issues.jboss.org/browse/JBIDE-14735 
 * 
 * @author Victor Rubezhny
 *
 */
public class PaletteInsertHelperTest extends TestCase {
	private static final String PROJECT_NAME = "StaticWebProject";  //$NON-NLS-1$
	private static final String PAGE_NAME = "/WebContent/html5test.html"; //$NON-NLS-1$
	private static final String TEMPLATE_START_TEXT = 
			"<a href=\"\" id=\"button-1\" data-role=\"button\">Link button</a>\n";  //$NON-NLS-1$
	private static final String BASE_LINE_TEXT = "<div data-role=\"content\">";  //$NON-NLS-1$

	// Test Data for JBIDE-15194
	private static final String PAGE_NAME_2 = "/WebContent/html5test2.html"; //$NON-NLS-1$
	private static final String TEMPLATE_START_TEXT_2 = 
			"<div data-role=\"page\" id=\"page-1\">\n" +	  	//$NON-NLS-1$ 
			"\t<div data-role=\"header\">\n" + 					//$NON-NLS-1$
			"\t\t<h1>Page Title</h1>\n" + 						//$NON-NLS-1$
			"\t</div>\n" +										//$NON-NLS-1$
			"\t<div data-role=\"content\">\n" +					//$NON-NLS-1$
			"\t\t<p>Page content goes here.</p>";  				//$NON-NLS-1$
	private static final String TEMPLATE_END_TEXT_2 = 
			"\t</div>\n\t<div data-role=\"footer\">\n\t\t<h4>Page Footer</h4>\n\t</div>\n</div>\n"; //$NON-NLS-1$
	private static final String BASE_LINE_TEXT_2 = "<div>";  //$NON-NLS-1$
	private static final String BASE_LINE_END_TEXT2 = "</div>";  //$NON-NLS-1$
	
	TestProjectProvider provider = null;
	protected IProject project = null;

	public void setUp() throws Exception {
		provider = new TestProjectProvider("org.jboss.tools.common.model.ui.test", null, PROJECT_NAME, false); 
		project = provider.getProject();
	}
	
	protected void tearDown() throws Exception {
		if(provider != null) {
			provider.dispose();
			project = null;
		}
	}
	
	public void testPaletteInsertHelper() {
		IFile testfile = project.getFile(PAGE_NAME);
		assertTrue("Test file doesn't exist: " + project.getName() + "/" + PAGE_NAME, 
				(testfile.exists() && testfile.isAccessible()));

		IEditorPart editor = WorkbenchUtils.openEditor(PROJECT_NAME + "/" + PAGE_NAME);
		assertNotNull("Cannot open an editor for the page: " + project.getName() + "/" + PAGE_NAME, 
				editor);
		try {
			ITextEditor textEditor = (ITextEditor)editor.getAdapter(ITextEditor.class);
			assertNotNull("Cannot open a text editor for the page: " + project.getName() + "/" + PAGE_NAME, 
					textEditor);
			IDocument document = (IDocument)editor.getAdapter(IDocument.class);
			assertNotNull("Cannot get a document for the page: " + project.getName() + "/" + PAGE_NAME, 
					textEditor);

			Properties pp = new Properties();
			pp.setProperty(PaletteInsertHelper.PROPERTY_START_TEXT, TEMPLATE_START_TEXT);
			pp.setProperty(PaletteInsertHelper.PROPERTY_END_TEXT, "");
			pp.setProperty(PaletteInsertHelper.PROPERTY_NEW_LINE, "true");
			pp.setProperty(PaletteInsertHelper.PROPERTY_REFORMAT_BODY, "yes");

			int tabWidth = getTabWidth();
			try {
				String text = document.get();
				int baseLineOffset = text.indexOf(BASE_LINE_TEXT);
				assertTrue("Base text line not found in document", baseLineOffset != -1);
				
				// Base line indent
				int baseLine = document.getLineOfOffset(baseLineOffset);
				String lineText = document.get(document.getLineOffset(baseLine), document.getLineLength(baseLine));
				String lineDelimiter = document.getLineDelimiter(baseLine);
				int baseLineInsetWidth = calculateDisplayedWidth(getIndentOfLine(lineText, lineDelimiter), tabWidth);
				assertTrue("Wrong test page template: base line inset must not be 0", baseLineInsetWidth > 0);
				
				// Next (empty) line template
				lineText = document.get(document.getLineOffset(baseLine + 1), document.getLineLength(baseLine + 1));
				assertTrue("Wrong test page template: empty line must not contain a text", lineText.trim().isEmpty());
				lineDelimiter = document.getLineDelimiter(baseLine + 1);
				int emptyLineInsetWidth = calculateDisplayedWidth(getIndentOfLine(lineText, lineDelimiter), tabWidth);
				assertTrue("Wrong test page template: empty line inset must not be greater than base line inset", emptyLineInsetWidth > baseLineInsetWidth);

				int index = text.indexOf('|');
				assertTrue("Insertion position not found in document", index != -1);
				
				try {
					document.replace(index, "|".length(), "");
				} catch (BadLocationException e) {
					fail("Cannot modify document: " + e.getLocalizedMessage());
				}
				textEditor.selectAndReveal(index, 0);
				
				// Insert template into current selection
				PaletteInsertHelper.getInstance().insertIntoEditor(textEditor, pp);
				
				String updatedText = document.get(); 
				int indexOfChange = updatedText.indexOf(TEMPLATE_START_TEXT.trim());
				assertTrue("Changed text line not found in document", indexOfChange != -1);
				
				int changedLine = document.getLineOfOffset(indexOfChange);
				lineText = document.get(document.getLineOffset(changedLine), document.getLineLength(changedLine));
				lineDelimiter = document.getLineDelimiter(changedLine);
				int changedLineInsetWidth = calculateDisplayedWidth(getIndentOfLine(lineText, lineDelimiter), tabWidth);
				assertTrue("Changed line indent must be greater than base line offset by value of tab width", changedLineInsetWidth - baseLineInsetWidth == tabWidth);
		
				// Repeat the insert operation in the next line
				int newIndex = document.getLineOffset(changedLine + 1);

				textEditor.selectAndReveal(newIndex, 0);

				// Insert template into current selection
				PaletteInsertHelper.getInstance().insertIntoEditor(textEditor, pp);
				
				updatedText = document.get(); 
				indexOfChange = updatedText.indexOf(TEMPLATE_START_TEXT.trim(), newIndex);
				assertTrue("Second changed text line not found in document", indexOfChange != -1);
				
				
				int secondChangedLine = document.getLineOfOffset(indexOfChange);
				lineText = document.get(document.getLineOffset(secondChangedLine), document.getLineLength(secondChangedLine));
				lineDelimiter = document.getLineDelimiter(secondChangedLine);
				int secondChangedLineInsetWidth = calculateDisplayedWidth(getIndentOfLine(lineText, lineDelimiter), tabWidth);
				assertTrue("Indent of second changed line must be the same as indent of first changed line", changedLineInsetWidth == secondChangedLineInsetWidth);
			} catch (BadLocationException e) {
				fail("Exception occured: " + e.getLocalizedMessage());
			}
		} finally {
			if (editor != null)
				WorkbenchUtils.closeAllEditors();
		}
	}
	
	/**
	 * Test case for issue JBIDE-15194
	 */
	public void testPaletteInsertHelperMultiline() {
		IFile testfile = project.getFile(PAGE_NAME_2);
		assertTrue("Test file doesn't exist: " + project.getName() + "/" + PAGE_NAME_2, 
				(testfile.exists() && testfile.isAccessible()));

		IEditorPart editor = WorkbenchUtils.openEditor(PROJECT_NAME + "/" + PAGE_NAME_2);
		assertNotNull("Cannot open an editor for the page: " + project.getName() + "/" + PAGE_NAME_2, 
				editor);
		try {
			ITextEditor textEditor = (ITextEditor)editor.getAdapter(ITextEditor.class);
			assertNotNull("Cannot open a text editor for the page: " + project.getName() + "/" + PAGE_NAME_2, 
					textEditor);
			IDocument document = (IDocument)editor.getAdapter(IDocument.class);
			assertNotNull("Cannot get a document for the page: " + project.getName() + "/" + PAGE_NAME_2, 
					textEditor);

			Properties pp = new Properties();
			pp.setProperty(PaletteInsertHelper.PROPERTY_START_TEXT, TEMPLATE_START_TEXT_2);
			pp.setProperty(PaletteInsertHelper.PROPERTY_END_TEXT, TEMPLATE_END_TEXT_2);
			pp.setProperty(PaletteInsertHelper.PROPERTY_NEW_LINE, "true");
			pp.setProperty(PaletteInsertHelper.PROPERTY_REFORMAT_BODY, "yes");

			int tabWidth = getTabWidth();
			try {
				String text = document.get();
				int baseLineOffset = text.indexOf(BASE_LINE_TEXT_2 + "|" + BASE_LINE_END_TEXT2);
				assertTrue("Base text line not found in document", baseLineOffset != -1);
				
				// Base line indent
				int baseLine = document.getLineOfOffset(baseLineOffset);
				String lineText = document.get(document.getLineOffset(baseLine), document.getLineLength(baseLine));
				String lineDelimiter = document.getLineDelimiter(baseLine);
				int baseLineInsetWidth = calculateDisplayedWidth(getIndentOfLine(lineText, lineDelimiter), tabWidth);
				assertTrue("Wrong test page template: base line inset must not be 0", baseLineInsetWidth > 0);

				int index = text.indexOf('|');
				assertTrue("Insertion position not found in document", index != -1);
				
				try {
					document.replace(index, "|".length(), "");
				} catch (BadLocationException e) {
					fail("Cannot modify document: " + e.getLocalizedMessage());
				}
				textEditor.selectAndReveal(index, 0);
				
				// Insert template into current selection
				PaletteInsertHelper.getInstance().insertIntoEditor(textEditor, pp);
				
				String updatedText = document.get(); 
				String[] templateStartLines = TEMPLATE_START_TEXT_2.split("\n");
				String[] templateEndLines = TEMPLATE_END_TEXT_2.split("\n");

				// Check the base line (it should be splitted into two lines)
				baseLineOffset = updatedText.indexOf(BASE_LINE_TEXT_2);
				assertTrue("Base text line not found in document", baseLineOffset != -1);
				baseLine = document.getLineOfOffset(baseLineOffset);
				baseLineInsetWidth = calculateDisplayedWidth(getIndentOfLine(lineText, lineDelimiter), tabWidth);
				String baseLineText = document.get(document.getLineOffset(baseLine), document.getLineLength(baseLine)).trim();
				assertTrue("Base line wasn't splitter or start of base line text isn't found",
						BASE_LINE_TEXT_2.trim().equals(baseLineText.trim()));

				int newIndex = document.getLineOffset(baseLine + 1);

				/// Check indent of first row of template start text and find the end of inserted template start text
				boolean doCheck = true;
				int changedLine = baseLine + 1;
				int secondChangedLineInsetWidth = -1;
				for (String line : templateStartLines) {
					lineText = document.get(document.getLineOffset(changedLine), document.getLineLength(changedLine));
					assertTrue("Changed text line from start template not found in document", 
							line.trim().equals(lineText.trim()));

					if (doCheck) {
						lineDelimiter = document.getLineDelimiter(changedLine);
						secondChangedLineInsetWidth = calculateDisplayedWidth(getIndentOfLine(lineText, lineDelimiter), tabWidth);
						assertTrue("Indent of second changed line must be greater than indent of first changed line by a tab width", 
								baseLineInsetWidth + tabWidth == secondChangedLineInsetWidth);
						doCheck = false;
					}
					changedLine++;
				}

				secondChangedLineInsetWidth = -1;
				// Check indent of last row of template end text and find the end of inserted template end text
				for (String line : templateEndLines) {
					lineText = document.get(document.getLineOffset(changedLine), document.getLineLength(changedLine));
					assertTrue("Changed text line from start template not found in document", 
							line.trim().equals(lineText.trim()));

					changedLine++;
				}

				// check the indent of the last line
				lineDelimiter = document.getLineDelimiter(changedLine - 1); // Because the index is incremented already
				secondChangedLineInsetWidth = calculateDisplayedWidth(getIndentOfLine(lineText, lineDelimiter), tabWidth);
				assertTrue("Indent of pre-last changed line must be greater than indent of first changed line by a tab width", 
						baseLineInsetWidth + tabWidth == secondChangedLineInsetWidth);
			
				// Check the indent of base line text ending (should be the same as the base line text starting one)
				lineText = document.get(document.getLineOffset(changedLine), document.getLineLength(changedLine));
				assertTrue("Changed text line from end template not found in document", 
						BASE_LINE_END_TEXT2.trim().equals(lineText.trim()));
				
				lineDelimiter = document.getLineDelimiter(changedLine);
				secondChangedLineInsetWidth = calculateDisplayedWidth(getIndentOfLine(lineText, lineDelimiter), tabWidth);
				assertTrue("Indent of last changed line must be greater than indent of first changed line by a tab width", 
						baseLineInsetWidth == secondChangedLineInsetWidth);
			} catch (BadLocationException e) {
				fail("Exception occured: " + e.getLocalizedMessage());
			}
		} finally {
			if (editor != null)
				WorkbenchUtils.closeAllEditors();
		}
	}
	
	public void testOffsetCorrectionToStartNode(){
		checkCorrectOffsetOrSelection("/WebContent/start_node.html", "<table", "");
	}
	
	public void testOffsetCorrectionToEndNode(){
		checkCorrectOffsetOrSelection("/WebContent/end_node.html", "END", "");
	}

	public void testSelectionCorrectionToNode(){
		checkCorrectOffsetOrSelection("/WebContent/full_node.html", "<td", "END");
	}

	public void testSelectionCorrectionExcludeNodes(){
		checkCorrectOffsetOrSelection("/WebContent/exclude_node.html", "<td", "END");
	}

	public void testSelectionCorrectionBetweenNodes(){
		checkCorrectOffsetOrSelection("/WebContent/between_nodes.html", "START", "");
	}
	
	public void testTextNode(){
		checkNotCorrectionOffsetOrSelection("/WebContent/test_text.html");
	}
	
	
	private void checkNotCorrectionOffsetOrSelection(String fileName) {
		IFile testfile = project.getFile(fileName);
		assertTrue("Test file doesn't exist: " + project.getName() + "/" + fileName, 
				(testfile.exists() && testfile.isAccessible()));

		IEditorPart editor = WorkbenchUtils.openEditor(PROJECT_NAME + "/" + fileName);
		assertNotNull("Cannot open an editor for the page: " + project.getName() + "/" + fileName, 
				editor);
		try {
			ITextEditor textEditor = (ITextEditor)editor.getAdapter(ITextEditor.class);
			assertNotNull("Cannot open a text editor for the page: " + project.getName() + "/" + fileName, 
					textEditor);
			IDocument document = (IDocument)editor.getAdapter(IDocument.class);
			assertNotNull("Cannot get a document for the page: " + project.getName() + "/" + fileName, 
					textEditor);

			try {
				String text = document.get();
				int startOffset = text.indexOf("|");
				assertTrue("First | marker not found", startOffset >= 0);
				document.replace(startOffset, 1, "");
				text = document.get();
				int endOffset = text.indexOf("|", startOffset);
				if(endOffset >= 0){
					document.replace(endOffset, 1, "");
				}else{
					endOffset = startOffset;
				}
				int length = endOffset - startOffset;
				
				for(int index = startOffset; index < endOffset; index++){
					int newOffset = PaletteInsertHelper.getInstance().correctOffset(document, index);
					
					assertEquals("Corrector did correct the offset", index, newOffset);
				}
					
				
			} catch (BadLocationException e) {
				fail("Exception occured: " + e.getLocalizedMessage());
			}
		} finally {
			if (editor != null)
				WorkbenchUtils.closeAllEditors();
		}
	}

	
	private void checkCorrectOffsetOrSelection(String fileName, String startTest, String endTest) {
		IFile testfile = project.getFile(fileName);
		assertTrue("Test file doesn't exist: " + project.getName() + "/" + fileName, 
				(testfile.exists() && testfile.isAccessible()));

		IEditorPart editor = WorkbenchUtils.openEditor(PROJECT_NAME + "/" + fileName);
		assertNotNull("Cannot open an editor for the page: " + project.getName() + "/" + fileName, 
				editor);
		try {
			ITextEditor textEditor = (ITextEditor)editor.getAdapter(ITextEditor.class);
			assertNotNull("Cannot open a text editor for the page: " + project.getName() + "/" + fileName, 
					textEditor);
			IDocument document = (IDocument)editor.getAdapter(IDocument.class);
			assertNotNull("Cannot get a document for the page: " + project.getName() + "/" + fileName, 
					textEditor);

			try {
				String text = document.get();
				int startOffset = text.indexOf("|");
				assertTrue("First | marker not found", startOffset >= 0);
				document.replace(startOffset, 1, "");
				text = document.get();
				int endOffset = text.indexOf("|", startOffset);
				if(endOffset >= 0){
					document.replace(endOffset, 1, "");
				}else{
					endOffset = startOffset;
				}
				int length = endOffset - startOffset;
				
				
				if(length == 0){
					int newOffset = PaletteInsertHelper.getInstance().correctOffset(document, startOffset);
					
					assertNotSame("Corrector did not correct the offset", startOffset, newOffset);
					
					String testString = document.get(newOffset, startTest.length());
					assertEquals("String not found for returned offset", startTest, testString);
				}else{
					ITextSelection newSelection = PaletteInsertHelper.getInstance().correctSelection(document, new TextSelection(document, startOffset, length));
					
					if(newSelection.getLength() == 0){
						assertTrue("Selection must be with 0 length", endTest.length() == 0);
					}
					
					String testStartString = document.get(newSelection.getOffset(), startTest.length());
					assertEquals("String not found for returned offset", startTest, testStartString);

					String testEndString = document.get(newSelection.getOffset()+newSelection.getLength(), endTest.length());
					assertEquals("String not found for returned offset", endTest, testEndString);
				}
				
			} catch (BadLocationException e) {
				fail("Exception occured: " + e.getLocalizedMessage());
			}
		} finally {
			if (editor != null)
				WorkbenchUtils.closeAllEditors();
		}
	}

	private int calculateDisplayedWidth(String string, int tabWidth) {

		int column= 0;
		for (int i= 0; i < string.length(); i++)
			if ('\t' == string.charAt(i))
				column += tabWidth - (column % tabWidth);
			else
				column++;

		return column;
	}

	private String getLineDelimiter(IDocument document, int line) throws BadLocationException {
		String delim = document.getLineDelimiter(line);
		return delim == null ? "" : delim;
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

    private static int getTabWidth() {
		return Platform.getPreferencesService().getInt("org.eclipse.ui.editors", AbstractDecoratedTextEditorPreferenceConstants.EDITOR_TAB_WIDTH, 4, new IScopeContext[]{new InstanceScope()});  //$NON-NLS-1$
	}
}
