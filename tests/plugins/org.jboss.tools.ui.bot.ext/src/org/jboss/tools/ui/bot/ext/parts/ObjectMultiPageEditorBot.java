package org.jboss.tools.ui.bot.ext.parts;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.Result;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

/**
 * 
 * @author jpeterka
 *
 */
public class ObjectMultiPageEditorBot {
	
	private final IEditorReference ref;
	
  public ObjectMultiPageEditorBot(String title) {
		ref = getEditorReferenceByTitle(title);
	}
	public ObjectMultiPageEditorBot(SWTBotEditor editor) {
		ref = editor.getReference();
	}
  
	public IEditorReference getEditorReference() {
    return ref;
  }
  private IEditorReference getEditorReferenceByTitle(final String title) {		
		// Search for editor
		IEditorReference ref = UIThreadRunnable.syncExec(new Result<IEditorReference>() {
			public IEditorReference run() {
				IEditorReference ref = null;
				IEditorReference[] editorReferences = null;
				editorReferences = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getEditorReferences();
				
				try {
					for (IEditorReference reference: editorReferences) {
						IEditorInput input = reference.getEditorInput();
						String name = input.getName();
						if (name.equals(title)) {
							return reference;
						}
					}
				} catch (PartInitException ex) {
					fail(ex.toString());
				}
				return ref;				
			}
		});
		
		assertNotNull(ref);
		return ref;
	}
	
	public void selectPage(final String pageName) {		
		
		final Object editor = ref.getPart(true);
		boolean found = false;
		for (Class<?> cl : editor.getClass().getDeclaredClasses()) {
			System.out.println(cl.getName());
			if (cl.getName().equals("org.jboss.tools.common.editor.ObjectMultiPageEditor")) {
				found=true;break;
			}
		}
		assertTrue("Editor is not instance of ObjectMultiPageEditor",found);
		try {
			final Method m = editor.getClass().getMethod("selectPageByName", String.class);
			// Select page
			Display.getDefault().syncExec(new Runnable() {
				
				public void run() {
					try {
						m.invoke(editor, pageName);						
					} catch (IllegalArgumentException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (IllegalAccessException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (InvocationTargetException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			});
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}

	