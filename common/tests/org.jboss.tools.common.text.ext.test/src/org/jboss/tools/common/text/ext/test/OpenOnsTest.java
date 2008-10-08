package org.jboss.tools.common.text.ext.test;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.FindReplaceDocumentAdapter;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.DocumentProviderRegistry;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.jboss.tools.common.model.ui.editor.EditorPartWrapper;
import org.jboss.tools.common.model.ui.editors.multipage.DefaultMultipageEditor;
import org.jboss.tools.common.model.ui.texteditors.XMLTextEditorStandAlone;
import org.jboss.tools.common.text.ext.hyperlink.CSSClassHyperlink;
import org.jboss.tools.common.text.ext.hyperlink.ClassHyperlink;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkDetector;
import org.jboss.tools.jst.jsp.jspeditor.JSPMultiPageEditor;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.ResourcesUtils;
import org.jboss.tools.test.util.WorkbenchUtils;

public class OpenOnsTest extends TestCase {

	public static final String STYLE_OPENON_PROJECT = "HiperlinksTestProject";


	public IProject project = null;

	protected void setUp() {
		project = ResourcesPlugin.getWorkspace().getRoot().getProject(
				STYLE_OPENON_PROJECT);
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().closeAllEditors(false);
		JobUtils.waitForIdle();
	}
	
	protected void tearDown() {
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().closeAllEditors(false);
	}

	public OpenOnsTest() {
		super("styleClass OpenOn tests");
	}
	
	public static final String WEB_XML_FILE_PATH = "WebContent/WEB-INF/web.xml";
	
	public void testFilterNameOpenOn() throws PartInitException, BadLocationException {
		IFile webXml = project.getFile(WEB_XML_FILE_PATH);
		IEditorDescriptor descriptor = IDE.getEditorDescriptor(webXml);
		IEditorPart editor = WorkbenchUtils.openEditor(webXml, descriptor.getId());
		editor = ((EditorPartWrapper)editor).getEditor();
		JobUtils.waitForIdle();
		DefaultMultipageEditor xmlMultyPageEditor = (DefaultMultipageEditor) editor;
		xmlMultyPageEditor.selectPageByName("Source");
		ISourceViewer viewer = xmlMultyPageEditor.getSourceEditor().getTextViewer(); 
			
		IDocument document = viewer.getDocument();
		IRegion reg = new FindReplaceDocumentAdapter(document).find(0,
				"Filter1", true, true, false, false);
		reg = new FindReplaceDocumentAdapter(document).find(reg.getOffset()+reg.getLength()+1,
				"Filter1", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		ITextSelection selection = (ITextSelection)viewer.getSelectionProvider().getSelection();
		assertEquals("<filter-name>", selection.getText());
	}
	
	public void testRoleNameOpenOn() throws PartInitException, BadLocationException {
		IFile webXml = project.getFile(WEB_XML_FILE_PATH);
		IEditorDescriptor descriptor = IDE.getEditorDescriptor(webXml);
		IEditorPart editor = WorkbenchUtils.openEditor(webXml, descriptor.getId());
		editor = ((EditorPartWrapper)editor).getEditor();
		JobUtils.waitForIdle();
		DefaultMultipageEditor xmlMultyPageEditor = (DefaultMultipageEditor) editor;
		xmlMultyPageEditor.selectPageByName("Source");
		ISourceViewer viewer = xmlMultyPageEditor.getSourceEditor().getTextViewer(); 
			
		IDocument document = viewer.getDocument();
		IRegion reg = new FindReplaceDocumentAdapter(document).find(0,
				"Designer", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		ITextSelection selection = (ITextSelection)viewer.getSelectionProvider().getSelection();
		assertEquals("<role-name>", selection.getText());
	}
	
	public void testServletNameOpenOn() throws PartInitException, BadLocationException {
		IFile webXml = project.getFile(WEB_XML_FILE_PATH);
		IEditorDescriptor descriptor = IDE.getEditorDescriptor(webXml);
		IEditorPart editor = WorkbenchUtils.openEditor(webXml, descriptor.getId());
		editor = ((EditorPartWrapper)editor).getEditor();
		JobUtils.waitForIdle();
		DefaultMultipageEditor xmlMultyPageEditor = (DefaultMultipageEditor) editor;
		xmlMultyPageEditor.selectPageByName("Source");
		ISourceViewer viewer = xmlMultyPageEditor.getSourceEditor().getTextViewer(); 
			
		IDocument document = viewer.getDocument();
		IRegion reg = new FindReplaceDocumentAdapter(document).find(0,
				"Faces Servlet", true, true, false, false);
		reg = new FindReplaceDocumentAdapter(document).find(reg.getOffset()+reg.getLength()+1,
				"Faces Servlet", true, true, false, false);
		reg = new FindReplaceDocumentAdapter(document).find(reg.getOffset()+reg.getLength()+1,
				"Faces Servlet", true, true, false, false);
		reg = new FindReplaceDocumentAdapter(document).find(reg.getOffset()+reg.getLength()+1,
				"Faces Servlet", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		ITextSelection selection = (ITextSelection)viewer.getSelectionProvider().getSelection();
		assertEquals("<servlet-name>", selection.getText());
	}

	public static final String TAGLIB_URI_TEST_FILE = "WebContent/tldUriHyperlinkTests.jsp";
	
	public void testTaglibUriFromJarOpenOn() throws BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(project
				.getFile(TAGLIB_URI_TEST_FILE), JSPMultiPageEditor.EDITOR_ID);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JobUtils.waitForIdle();
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();
		IDocument document = jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument();
		IRegion reg = new FindReplaceDocumentAdapter(document).find(0,
				"jsf/core", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		System.out.println(fileName);
		assertTrue("jsf_core.tld".equals(fileName));
	}
	public static final String TAGLIB_URI_JSP_ROOT_TEST_FILE = "WebContent/jspTagsHyperlinkTests.jsp";
	
	public void testTaglibUriFromJarinJspRootOpenOn() throws BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(project
				.getFile(TAGLIB_URI_JSP_ROOT_TEST_FILE), JSPMultiPageEditor.EDITOR_ID);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JobUtils.waitForIdle();
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();
		IDocument document = jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument();
		IRegion reg = new FindReplaceDocumentAdapter(document).find(0,
				"jsf/core", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		System.out.println(fileName);
		assertTrue("jsf_core.tld".equals(fileName));
	}
	
	public static final String STYLE_TEST_FILE = "WebContent/styleHyperlinkTests.jsp";
	public static final String CSS1_TEST_FILE = "WebContent/stylesheet/style1.css";
	public static final String CSS2_TEST_FILE = "WebContent/stylesheet/style2.css";
	
	public void testStylesheetOpenOn() throws BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(project
				.getFile(STYLE_TEST_FILE), JSPMultiPageEditor.EDITOR_ID);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JobUtils.waitForIdle();
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();
		IDocument document = jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument();
		IRegion reg = new FindReplaceDocumentAdapter(document).find(0,
				"stylesheet/style1.css", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		assertTrue("style1.css".equals(fileName));		
	}
	
	public void testStyleClassOpenOns() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(project
				.getFile(STYLE_TEST_FILE), JSPMultiPageEditor.EDITOR_ID);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JobUtils.waitForIdle();
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();
		IDocument document = jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument();
		IRegion reg = new FindReplaceDocumentAdapter(document).find(0,
				"style-class9\"", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		assertTrue("styleHyperlinkTests.jsp".equals(fileName));
		
		reg = new FindReplaceDocumentAdapter(document).find(0,
				"style-class3", true, true, false, false);
		links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		fileName = editor.getEditorInput().getName();
		assertTrue("style1.css".equals(fileName));
		
		reg = new FindReplaceDocumentAdapter(document).find(0,
				"style-class6", true, true, false, false);
		links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		links[0].open();
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		fileName = editor.getEditorInput().getName();
		assertTrue("style2.css".equals(fileName));
	}
	
	public static final String CLASS_TEST_FILE = "WebContent/classHyperlinkTests.jsp";
	public static final String CLASS1_TEST_FILE = "JavaSource/org/jboss/test/ChangeListenerInstance.java";
	
	public void testClassNameOpenOn() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(project
				.getFile(CLASS_TEST_FILE), JSPMultiPageEditor.EDITOR_ID);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();

		IRegion reg = new FindReplaceDocumentAdapter(jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument()).find(0,
				"org.jboss.tools.test.ChangeListenerInstance", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		assertTrue("ChangeListenerInstance.java".equals(fileName));
	}
	
	public void testTaglibTagNameOpenOn() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(project
				.getFile(CLASS_TEST_FILE), JSPMultiPageEditor.EDITOR_ID);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();

		IRegion reg = new FindReplaceDocumentAdapter(jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument()).find(0,
				"view", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		System.out.println(links[0].getClass().getName());
		links[0].open();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		assertTrue("jsf_core.tld".equals(fileName));
	}
	
	
}
