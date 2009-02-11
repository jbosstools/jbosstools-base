/**
 * 
 */
package org.jboss.tools.common.core.jdt.test;

import java.util.List;

import junit.framework.TestCase;

import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.core.jdt.FavoritesClassController;

/**
 * @author eskimo
 *
 */
public class FavoritesClassControllerTest extends TestCase {

	/**
	 * Test method for {@link org.jboss.tools.common.core.jdt.FavoritesClassController#getFavoritesClassesList()}.
	 */
	public void testGetFavoritesClassesList() {
		List<String> favClasses = FavoritesClassController.getFavoritesClassesList();
		assertEquals(0, favClasses.size());
		FavoritesClassController.push("corg.jboss.TestClass1");
		FavoritesClassController.push("corg.jboss.TestClass2");
		FavoritesClassController.push("corg.jboss.TestClass3");
		FavoritesClassController.push("corg.jboss.TestClass4");
		FavoritesClassController.push("corg.jboss.TestClass5");
		favClasses = FavoritesClassController.getFavoritesClassesList();
		assertEquals(5, favClasses.size());
	}

	/**
	 * Test method for {@link org.jboss.tools.common.core.jdt.FavoritesClassController#push(java.lang.String)}.
	 */
	public void testPush() {
		FavoritesClassController.push("corg.jboss.TestClass1");
		List<String> favClasses = FavoritesClassController.getFavoritesClassesList();
		assertEquals("corg.jboss.TestClass1", favClasses.get(0));
		FavoritesClassController.push("corg.jboss.TestClass5");
		favClasses = FavoritesClassController.getFavoritesClassesList();
		assertEquals("corg.jboss.TestClass5", favClasses.get(0));
		FavoritesClassController.push("corg.jboss.TestClass6");
		favClasses = FavoritesClassController.getFavoritesClassesList();
		assertEquals("corg.jboss.TestClass6", favClasses.get(0));
	}

	/**
	 * Test method for {@link org.jboss.tools.common.core.jdt.FavoritesClassController#getLabelProvider()}.
	 */
	public void testGetLabelProvider() {
		Image img1 = FavoritesClassController.getLabelProvider().getImage(new Object());
		Image img2 = FavoritesClassController.getLabelProvider().getImage(new Object());
		assertTrue(img1==img2);
	}

}
