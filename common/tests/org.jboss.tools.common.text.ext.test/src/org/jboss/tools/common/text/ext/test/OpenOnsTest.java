package org.jboss.tools.common.text.ext.test;

import junit.framework.TestCase;

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
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.model.ui.editor.EditorPartWrapper;
import org.jboss.tools.common.model.ui.editors.multipage.DefaultMultipageEditor;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkDetector;
import org.jboss.tools.jst.jsp.jspeditor.JSPMultiPageEditor;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.WorkbenchUtils;

public class OpenOnsTest extends TestCase {

	public static final String OPENON_TEST_PROJECT = "HiperlinksTestProject";


	public IProject project = null;

	protected void setUp() {
		project = ResourcesPlugin.getWorkspace().getRoot().getProject(
				OPENON_TEST_PROJECT);
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().closeAllEditors(false);
		JobUtils.waitForIdle();
		IWorkbench workbench = PlatformUI.getWorkbench();
	}
	
	protected void tearDown() {
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().closeAllEditors(false);
	}

	public OpenOnsTest() {
		super("styleClass OpenOn tests");
	}
	
	public static final String WEB_XML_FILE_PATH = OPENON_TEST_PROJECT+"/WebContent/WEB-INF/web.xml";
	
	public void testFilterNameOpenOn() throws PartInitException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(WEB_XML_FILE_PATH);
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
		IEditorPart editor = WorkbenchUtils.openEditor(WEB_XML_FILE_PATH);
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
		IEditorPart editor = WorkbenchUtils.openEditor(WEB_XML_FILE_PATH);
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

	public static final String TAGLIB_URI_TEST_FILE = OPENON_TEST_PROJECT + "/WebContent/tldUriHyperlinkTests.jsp";
	
	public void testTaglibUriFromJarOpenOn() throws BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(TAGLIB_URI_TEST_FILE);
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
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		System.out.println(fileName);
		assertTrue("jsf_core.tld".equals(fileName));
	}
	public static final String TAGLIB_URI_JSP_ROOT_TEST_FILE = OPENON_TEST_PROJECT + "/WebContent/jspTagsHyperlinkTests.jsp";
	
	public void testTaglibUriFromJarinJspRootOpenOn() throws BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(TAGLIB_URI_JSP_ROOT_TEST_FILE);
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
		JobUtils.waitForIdle();
		
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		System.out.println(fileName);
		assertTrue("jsf_core.tld".equals(fileName));
	}
	
	public static final String STYLE_TEST_FILE = OPENON_TEST_PROJECT + "/WebContent/styleHyperlinkTests.jsp";
	public static final String CSS1_TEST_FILE = OPENON_TEST_PROJECT + "/WebContent/stylesheet/style1.css";
	public static final String CSS2_TEST_FILE = OPENON_TEST_PROJECT + "/WebContent/stylesheet/style2.css";
	
	public void testStylesheetOpenOn() throws BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(STYLE_TEST_FILE);
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
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		assertTrue("style1.css".equals(fileName));		
	}
	
	public void testStyleClassOpenOns() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(STYLE_TEST_FILE);
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
		JobUtils.waitForIdle();
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
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		fileName = editor.getEditorInput().getName();
		assertTrue("style1.css".equals(fileName));
		
		reg = new FindReplaceDocumentAdapter(document).find(0,
				"style-class6", true, true, false, false);
		links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		links[0].open();
		JobUtils.waitForIdle();
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		fileName = editor.getEditorInput().getName();
		assertTrue("style2.css".equals(fileName));
	}
	
	public static final String CLASS_TEST_FILE = OPENON_TEST_PROJECT + "/WebContent/classHyperlinkTests.jsp";
	public static final String CLASS1_TEST_FILE = OPENON_TEST_PROJECT + "/JavaSource/org/jboss/test/ChangeListenerInstance.java";
	
	public void testClassNameOpenOn() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(CLASS_TEST_FILE);
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
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		assertTrue("ChangeListenerInstance.java".equals(fileName));
	}
	
	public void testTaglibTagNameOpenOn() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(CLASS_TEST_FILE);
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
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		assertTrue("jsf_core.tld".equals(fileName));
	}
	
	public static final String USE_BEAN_TEST_FILE = OPENON_TEST_PROJECT + "/WebContent/beanHyperlinkTests.jsp";
	
	public void testUseBeanClassOpenOn() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(USE_BEAN_TEST_FILE);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();

		IRegion reg = new FindReplaceDocumentAdapter(jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument()).find(0,
				"b1", true, true, false, false);
		reg = new FindReplaceDocumentAdapter(jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument()).find(reg.getOffset()+reg.getLength(),
				"b1", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		ITextSelection selection = (ITextSelection)viewer.getSelectionProvider().getSelection();
		assertEquals("<jsp:useBean id=\"b1\" class=\"org.jboss.tools.test.TestBean1\">", selection.getText());
		
		reg = new FindReplaceDocumentAdapter(jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument()).find(0,
				"org.jboss.tools.test.TestBean1", true, true, false, false);
		links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		String fileName = editor.getEditorInput().getName();
		assertTrue("TestBean1.java".equals(fileName));
	}

	public void testGetBeanPropertyOpenOn() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(USE_BEAN_TEST_FILE);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();

		IRegion reg = new FindReplaceDocumentAdapter(jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument()).find(0,
				"property1", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		JobUtils.waitForIdle();
	
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		String fileName = editor.getEditorInput().getName();
		assertTrue("TestBean1.java".equals(fileName));		
	}
	
	public void testSetBeanPropertyOpenOn() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(USE_BEAN_TEST_FILE);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();

		IRegion reg = new FindReplaceDocumentAdapter(jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument()).find(0,
				"property2", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		JobUtils.waitForIdle();
	
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		String fileName = editor.getEditorInput().getName();
		assertTrue("TestBean1.java".equals(fileName));		
	}
	
	public static final String FORWARD_TEST_FILE = OPENON_TEST_PROJECT + "/WebContent/forwardHiperlinkTests.jsp";

	
	public void testJspForwardOpenOn() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(FORWARD_TEST_FILE);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();

		IRegion reg = new FindReplaceDocumentAdapter(viewer.getDocument()).find(0,
				"forward/forwardHiperlinkPage2Tests.jsp", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		assertTrue("forwardHiperlinkPage2Tests.jsp".equals(fileName));
		
		reg = new FindReplaceDocumentAdapter(jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument()).find(0,
				"forwardHiperlinkPage1Tests.jsp", true, true, false, false);
		links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		JobUtils.waitForIdle();
		
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		fileName = editor.getEditorInput().getName();
		assertTrue("forwardHiperlinkPage1Tests.jsp".equals(fileName));
	}
	
	public static final String INCLUDE_TEST_FILE = OPENON_TEST_PROJECT + "/WebContent/includeHiperlinkTests.jsp";

	
	public void testJspIncludeOpenOn() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(INCLUDE_TEST_FILE);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();

		IRegion reg = new FindReplaceDocumentAdapter(jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument()).find(0,
				"include/includeHiperlinkPage2Tests.jsp", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		assertTrue("includeHiperlinkPage2Tests.jsp".equals(fileName));
		
		reg = new FindReplaceDocumentAdapter(jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument()).find(0,
				"includeHiperlinkPage1Tests.jsp", true, true, false, false);
		links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		fileName = editor.getEditorInput().getName();
		assertTrue("includeHiperlinkPage1Tests.jsp".equals(fileName));
	}
	
	public static final String FACELETS_XHTML_TEST_FILE = OPENON_TEST_PROJECT + "/WebContent/faceletsHiperlinkTests.xhtml";

	
	public void testJspXmlElementNameOpenOn() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(FACELETS_XHTML_TEST_FILE);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();

		IRegion reg = new FindReplaceDocumentAdapter(jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument()).find(0,
				"message", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		assertTrue("html_basic.tld".equals(fileName));
	}
	
	public static final String JSP_XMLNS_TEST_FILE = OPENON_TEST_PROJECT + "/WebContent/jspXmlFormatTests.jsp";

	
	public void testJspXmlnsNameOpenOn() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(FACELETS_XHTML_TEST_FILE);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();

		IRegion reg = new FindReplaceDocumentAdapter(jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument()).find(0,
				"http://java.sun.com/jsf/html", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		assertTrue("html_basic.tld".equals(fileName));
	}

	public static final String TLD_ATTRIBUTE_NAME_TEST_FILE = OPENON_TEST_PROJECT + "/WebContent/WEB-INF/tldAttributeNameOpenOnTests.tld";

	public void testTldAttributeNameOpenOn() throws CoreException, BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(TLD_ATTRIBUTE_NAME_TEST_FILE);
		assertTrue(editor instanceof DefaultMultipageEditor);
		DefaultMultipageEditor tldEditor = (DefaultMultipageEditor) editor;
		tldEditor.selectPageByName("Source");
		ISourceViewer viewer = tldEditor.getSourceEditor().getTextViewer();

		IRegion reg = new FindReplaceDocumentAdapter(tldEditor.getSourceEditor().getTextViewer().getDocument()).find(0,
				"attr1", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		ITextSelection selection = (ITextSelection)viewer.getSelectionProvider().getSelection();
		assertEquals("<name>", selection.getText());
	}
	
	public static final String XHTML_STYLE_CLASS_NAME_TEST_FILE = OPENON_TEST_PROJECT + "/WebContent/xhtmlStyleClassHiperlinkTests.xhtml";

	
	public void testFacletsStyleClassOpenOnJbide2890() throws BadLocationException {
		IEditorPart editor = WorkbenchUtils.openEditor(XHTML_STYLE_CLASS_NAME_TEST_FILE);
		assertTrue(editor instanceof JSPMultiPageEditor);
		JSPMultiPageEditor jspMultyPageEditor = (JSPMultiPageEditor) editor;
		ISourceViewer viewer = jspMultyPageEditor.getSourceEditor().getTextViewer();

		IRegion reg = new FindReplaceDocumentAdapter(jspMultyPageEditor.getSourceEditor().getTextViewer().getDocument()).find(0,
				"style-class1", true, true, false, false);
		IHyperlink[] links = HyperlinkDetector.getInstance().detectHyperlinks(viewer, reg, false);
		assertNotNull(links);
		assertTrue(links.length!=0);
		//assertNotNull(links[0].getHyperlinkText());
		assertNotNull(links[0].toString());
		links[0].open();
		JobUtils.waitForIdle();
		editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		
		String fileName = editor.getEditorInput().getName();
		assertTrue("style1.css".equals(fileName));		
	}
}
