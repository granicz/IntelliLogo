document.addEventListener('DOMContentLoaded', function () {
    const splitter = document.querySelector('.splitter');
    let isDragging = false;

    const startDrag = function (e) {
        e.preventDefault(); // Prevent default action (scroll / selection)
        isDragging = true;
        // Add both mouse and touch move/end listeners
        document.addEventListener('mousemove', onDrag);
        document.addEventListener('mouseup', stopDrag);
        document.addEventListener('touchmove', onDrag, {passive: false});
        document.addEventListener('touchend', stopDrag);
    };

    const onDrag = function (e) {
        if (!isDragging) return;
        let clientX = e.clientX || e.touches[0].clientX;
        const parentRect = splitter.parentNode.getBoundingClientRect();
        const leftWidth = clientX - parentRect.left;
        const rightWidth = parentRect.right - clientX;
        splitter.previousElementSibling.style.width = `${leftWidth}px`;
        splitter.nextElementSibling.style.width = `${rightWidth}px`;
    };

    const stopDrag = function (e) {
        if (isDragging) {
            isDragging = false;
            document.removeEventListener('mousemove', onDrag);
            document.removeEventListener('mouseup', stopDrag);
            document.removeEventListener('touchmove', onDrag);
            document.removeEventListener('touchend', stopDrag);
        }
    };

    // Listen for both mouse and touch start events
    splitter.addEventListener('mousedown', startDrag);
    splitter.addEventListener('touchstart', startDrag, {passive: false});
});

document.addEventListener('DOMContentLoaded', () => {
    const verticalSplitter = document.querySelector('.splitter-v');
    let isDragging = false;

    verticalSplitter.addEventListener('mousedown', function(e) {
        e.preventDefault(); // prevent text selection
    isDragging = true;
    document.addEventListener('mousemove', onDrag);
    document.addEventListener('mouseup', stopDrag);
    });

    function onDrag(e) {
        if (!isDragging) return;
    const parent = verticalSplitter.parentNode;
    const rect = parent.getBoundingClientRect();
    const y = e.clientY - rect.top; // y position within the parent
    const topHeight = y;
    const bottomHeight = rect.height - y - verticalSplitter.offsetHeight;
    verticalSplitter.previousElementSibling.style.flexGrow = 0;
    verticalSplitter.previousElementSibling.style.height = `${topHeight}px`;
    verticalSplitter.nextElementSibling.style.flexGrow = 0;
    verticalSplitter.nextElementSibling.style.height = `${bottomHeight}px`;
    }

    function stopDrag() {
        if (isDragging) {
        document.removeEventListener('mousemove', onDrag);
    document.removeEventListener('mouseup', stopDrag);
    isDragging = false;
        }
    }
});
