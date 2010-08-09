package org.jboss.tools.usage.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.net.HttpURLConnection;

import org.jboss.tools.usage.jgoogleanalytics.HttpGetMethod;
import org.junit.Test;

public class JBossToolsUsageRequestsTest {

	private static final String GOOGLE_ANALYTICS_TRACKING_URL = "http://www.google-analytics.com/__utm.gif";

	@Test
	public void testUrl1() throws IOException {
		String userAgent = "com.jboss.jbds.product/3.5.0 (gtk; U; linux x86; en_US) v20090525 Eclipse/3.5.0.v20090525";
		TestHttpGetMethod method = new TestHttpGetMethod(userAgent);
		String url = GOOGLE_ANALYTICS_TRACKING_URL
		+ "?" 
		+"utmwv=1"
		+ "&utmn=2016784548"
		+ "&utmcs=UTF-8"
		+ "&utmsc=24-bit"
		+ "&utmsr=1920x1080"
		+ "&utmul=en_US"
		+ "&utmcr=1"
		+ "&utmdt=jboss.org-tools-usage-action-wsstartup-test-"
		+ "&utmhn=localhost"
		+ "&utmr=org.jboss.tools.usage.tests"
		+ "&utmp=/jboss.org/tools/usage/action/wsstartup/testUrl1"
		+ "&utmac=UA-17645367-1"
		+ "&utmcc=__utma%3D'2131379572.1097452543.1280502756059.1280502756059.1280502756059.2%3B%2B__utmb%3D649997112%3B%2B__utmc%3D2002669685%3B%2B__utmz%3D1137436918.1280502756059";

		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

	@Test
	public void testUrl1_Session2() throws IOException {
		String userAgent = "com.jboss.jbds.product/3.0.1 (gtk; U; linux x86; en_US) v20090525 Eclipse/3.5.0.v20090525";
		TestHttpGetMethod method = new TestHttpGetMethod(userAgent);
		String url = GOOGLE_ANALYTICS_TRACKING_URL
		+ "?" 
		+"utmwv=1"
		+ "&utmn=2016784548"
		+ "&utmcs=UTF-8"
		+ "&utmsc=24-bit"
		+ "&utmsr=1920x1080"
		+ "&utmul=en_US"
		+ "&utmcr=1"
		+ "&utmdt=jboss.org-tools-usage-action-wsstartup-test-"
		+ "&utmhn=localhost"
		+ "&utmr=org.jboss.tools.usage.tests"
		+ "&utmp=/jboss.org/tools/usage/action/wsstartup/testUrl1_Session2"
		+ "&utmac=UA-17645367-1"
		+ "&utmcc=__utma%3D'2131379572.1097452543.1280502756059.1280502756059.1280502756059.2%3B%2B__utmb%3D649997112%3B%2B__utmc%3D2002669685%3B%2B__utmz%3D1137436918.1280502756059";

		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

	@Test
	public void testUrl2() throws IOException {
		String userAgent = "com.jboss.jbds.product/3.0.1 (X11; U; linux x86; en_US) v20090525 Eclipse/3.5.0.v20090525";
		TestHttpGetMethod method = new TestHttpGetMethod(userAgent);
		String url = GOOGLE_ANALYTICS_TRACKING_URL
		+ "?" 
		+"utmwv=1"
		+ "&utmn=2016784548"
		+ "&utmcs=UTF-8"
		+ "&utmsc=24-bit"
		+ "&utmsr=1920x1080"
		+ "&utmul=en_US"
		+ "&utmcr=1"
		+ "&utmdt=jboss.org-tools-usage-action-wsstartup-test-"
		+ "&utmhn=localhost"
		+ "&utmr=org.jboss.tools.usage.tests"
		+ "&utmp=/jboss.org/tools/usage/action/wsstartup/testUrl2"
		+ "&utmac=UA-17645367-1"
		+ "&utmcc=__utma%3D'2131379572.1097452543.1280502756059.1280502756059.1280502756059.2%3B%2B__utmb%3D649997112%3B%2B__utmc%3D2002669685%3B%2B__utmz%3D1137436918.1280502756059";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

	@Test
	public void testUrl3() throws IOException {
		String userAgent = "com.jboss.jbds.product/3.0.1 (X11; linux x86; en_US) v20090525 Eclipse/3.5.0";
		TestHttpGetMethod method = new TestHttpGetMethod(userAgent);
		String url = GOOGLE_ANALYTICS_TRACKING_URL
		+ "?" 
		+"utmwv=1"
		+ "&utmn=2016784548"
		+ "&utmcs=UTF-8"
		+ "&utmsc=24-bit"
		+ "&utmsr=1920x1080"
		+ "&utmul=en_US"
		+ "&utmcr=1"
		+ "&utmdt=jboss.org-tools-usage-action-wsstartup-test-"
		+ "&utmhn=localhost"
		+ "&utmr=org.jboss.tools.usage.tests"
		+ "&utmp=/jboss.org/tools/usage/action/wsstartup/testUrl3"
		+ "&utmac=UA-17645367-1"
		+ "&utmcc=__utma%3D'2131379572.1097452543.1280502756059.1280502756059.1280502756059.2%3B%2B__utmb%3D649997112%3B%2B__utmc%3D2002669685%3B%2B__utmz%3D1137436918.1280502756059";

		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

	protected class TestHttpGetMethod extends HttpGetMethod {

		private HttpURLConnection urlConnection;


		public TestHttpGetMethod(String userAgent) {
			super(userAgent);
		}

		@Override
		protected HttpURLConnection createURLConnection(String urlString,
				String userAgent) throws IOException {
			this.urlConnection = super.createURLConnection(urlString, userAgent);
			return urlConnection;
		}

		public int getResponseCode() throws IOException {
			return super.getResponseCode(urlConnection);
		}
	}
	
}
