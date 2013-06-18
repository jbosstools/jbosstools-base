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
		ITextEditor textEditor = (ITextEditor)editor.getAdapter(ITextEditor.class);
		IDocument document = (IDocument)editor.getAdapter(IDocument.class);
		
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
