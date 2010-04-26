package org.jboss.tools.common.model.ui.jarproperties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.internal.core.JarEntryDirectory;
import org.eclipse.jdt.internal.core.JarEntryFile;
import org.eclipse.jdt.internal.core.JarEntryResource;
import org.eclipse.jdt.internal.ui.javaeditor.JarEntryEditorInput;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.propertieseditor.PropertiesCompoundEditor;
import org.jboss.tools.common.test.util.TestProjectProvider;

import junit.framework.TestCase;

public class JarPropertiesTest extends TestCase {
	private static final String PROJECT_NAME = "TestJarProperties";
	TestProjectProvider provider = null;
	protected IProject project = null;

	public void setUp() throws Exception {
		provider = new TestProjectProvider("org.jboss.tools.common.model.ui.test", null, PROJECT_NAME, false); 
		project = provider.getProject();
	}

	public void testOpenJarProperties() {
		IResource jar = project.getFile("/lib/test.jar");
		JarEntryFile f = getJarEntryFileForResource(project, jar, "org.test", "LogStrings.properties");
		IWorkbenchPage page = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
		IEditorInput input = new JarEntryEditorInput(f);
		try {
			IEditorPart editor = page.openEditor(input, "org.jboss.tools.common.propertieseditor.PropertiesCompoundEditor");
			assertEquals(editor.getClass().getName(), PropertiesCompoundEditor.class.getName());
			PropertiesCompoundEditor p = (PropertiesCompoundEditor)editor;
			IEditorInput e1 = p.getEditorInput();
			if(e1 instanceof IModelObjectEditorInput) {
				IModelObjectEditorInput moei = (IModelObjectEditorInput)e1;
				assertNotNull(moei.getXModelObject());
			} else {
				fail("Editor input is not instanceof IModelObjectEditorInput: " + e1.getClass().getName());
			}
			
		} catch (PartInitException e) {
			fail(e.getMessage());
		}
	}

	JarEntryFile getJarEntryFileForResource(IProject p, IResource file, String packageName, String fileName) {
		JarEntryFile f = new JarEntryFile(fileName);
		JarEntryResource current = f;

        IJavaProject jp = EclipseResourceUtil.getJavaProject(p);        
        if(jp == null) return null;
        
        IPackageFragmentRoot root = jp.getPackageFragmentRoot(file);
        if(root == null) return null;

		if(current != null && !"META-INF".equalsIgnoreCase(current.getName()) && packageName.length() > 0) {
			IPackageFragment pf = root.getPackageFragment(packageName);
			f.setParent(pf);
		} else {		
			current.setParent(root);
		}
		
		return f;
	}
	


	protected void tearDown() throws Exception {
		if(provider != null) {
			provider.dispose();
			project = null;
		}
	}

}
